namespace LambdaCalculus;

public partial class Interpreter
{
    public async Task RunInteractiveLoopAsync()
        => await InputLoopAsync(async input => await ProcessAndDisplayInputAsync(input!));

    // Helper method to safely load a file if it exists
    public async Task<string> LoadFileIfExistsAsync(string path)
    {
        if (!File.Exists(path))
            return $"File not found: {path}";
        try { return await LoadFileAsync(path); }
        catch (Exception ex) { return $"Error loading {path}: {ex.Message}"; }
    }

    public async Task<string> LoadFileAsync(string path)
    {
        int lineCount = 0;
        _logger.Log($"Loading commands from '{path}'");
        var lines = await File.ReadAllLinesAsync(path);
        var currentInput = new System.Text.StringBuilder();
        foreach (var line in lines)
            await ProcessLineWithContinuation(line, currentInput, ProcessAndDisplayInputAsync, true, lineCount++);
        if (currentInput.Length > 0)
            await ProcessAndDisplayInputAsync(currentInput.ToString());
        return $"Loaded {path}";
    }

    private async Task InputLoopAsync(Func<string?, Task> handleInput, string promptPrimary = "lambda> ", string promptCont = "......> ")
    {
        var currentInput = new System.Text.StringBuilder();
        var lineCount = 0;
        while (true)
        {
            if (currentInput.Length == 0)
            {
                Console.WriteLine();
                lineCount = 0;
            }
            var prompt = currentInput.Length == 0 ? promptPrimary : $"{promptCont}[{lineCount + 1}] ";
            var indentation = GetSmartIndentation(currentInput.ToString());
            if (currentInput.Length > 0 && indentation.Length > 0)
                prompt += indentation;
            Console.Write(Logger.Prompt(prompt));
            var line = Console.ReadLine();
            if (line is null)
            {
                if (currentInput.Length > 0)
                {
                    Console.WriteLine("\nIncomplete input discarded.");
                    currentInput.Clear();
                    continue;
                }
                Console.WriteLine("\nGoodbye!");
                break;
            }
            if (currentInput.Length > 0)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine == ":cancel" || trimmedLine == ":abort")
                {
                    Console.WriteLine("Multi-line input cancelled.");
                    currentInput.Clear();
                    lineCount = 0;
                    continue;
                }
                if (trimmedLine == ":show")
                {
                    Console.WriteLine("Current input:");
                    Console.WriteLine(FormatMultiLineInput(currentInput.ToString()));
                    continue;
                }
                if (string.IsNullOrWhiteSpace(line) && trimmedLine == "")
                {
                    var input = currentInput.ToString().Trim();
                    if (!string.IsNullOrEmpty(input))
                    {
                        currentInput.Clear();
                        lineCount = 0;
                        await handleInput(input);
                        if (input.Trim() is ":exit" or ":quit") break;
                        continue;
                    }
                }
            }
            if (string.IsNullOrWhiteSpace(line) && currentInput.Length == 0) continue;
            await _logger.LogAsync($"λ> {line}", false);
            lineCount++;
            if (line.EndsWith('\\'))
            {
                currentInput.Append(line[..^1]);
                currentInput.Append(' ');
                continue;
            }
            if (currentInput.Length > 0) currentInput.Append(' ');
            currentInput.Append(line.Trim());
            var fullInput = currentInput.ToString();
            if (!IsInputComplete(fullInput)) continue;
            currentInput.Clear();
            lineCount = 0;
            await handleInput(fullInput);
            if (fullInput.Trim() is ":exit" or ":quit") break;
        }
    }

    private async Task ProcessLineWithContinuation(string line, System.Text.StringBuilder currentInput, Func<string, Task> processInput, bool isFromFile = false, int lineNumber = 0)
    {
        if (isFromFile) await _logger.LogAsync($"line {lineNumber} <<: {line}");
        var trimmed = isFromFile ? line.TrimEnd() : line;
        if (isFromFile && (string.IsNullOrWhiteSpace(trimmed) || trimmed.StartsWith('#'))) return;
        if (trimmed.EndsWith('\\'))
        {
            currentInput.Append(trimmed[..^1]);
            if (!isFromFile) currentInput.Append(' ');
            return;
        }
        if (currentInput.Length > 0) currentInput.Append(' ');
        currentInput.Append(trimmed);
        var input = currentInput.ToString();
        if (!isFromFile && !IsInputComplete(input)) return;
        currentInput.Clear();
        await processInput(input);
    }

    private string GetSmartIndentation(string currentInput)
    {
        if (string.IsNullOrWhiteSpace(currentInput)) return "";
        try
        {
            var tokens = _parser.Tokenize(currentInput);
            var parenDepth = 0; var bracketDepth = 0; var inLambda = false; var inLet = false;
            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case TokenType.LParen: parenDepth++; break;
                    case TokenType.RParen: parenDepth--; break;
                    case TokenType.LBracket: bracketDepth++; break;
                    case TokenType.RBracket: bracketDepth--; break;
                    case TokenType.Lambda: inLambda = true; break;
                    case TokenType.Let: inLet = true; break;
                    case TokenType.Dot when inLambda: return "  ";
                    case TokenType.In when inLet: return "  ";
                }
            }
            var totalDepth = Math.Max(0, parenDepth + bracketDepth);
            return new string(' ', totalDepth * 2);
        }
        catch { return ""; }
    }

    private string FormatMultiLineInput(string input)
    {
        if (string.IsNullOrWhiteSpace(input)) return input;
        var lines = new List<string>();
        var currentLine = new System.Text.StringBuilder();
        var depth = 0;
        foreach (char c in input)
        {
            switch (c)
            {
                case '(': case '[': currentLine.Append(c); depth++; break;
                case ')': case ']': depth--; currentLine.Append(c); break;
                case ' ' when currentLine.Length > 60:
                    lines.Add(new string(' ', depth * 2) + currentLine.ToString());
                    currentLine.Clear();
                    break;
                default: currentLine.Append(c); break;
            }
        }
        if (currentLine.Length > 0)
            lines.Add(new string(' ', depth * 2) + currentLine.ToString());
        return string.Join('\n', lines);
    }

    private bool IsInputComplete(string input)
    {
        if (string.IsNullOrWhiteSpace(input)) return true;
        try
        {
            var tokens = _parser.Tokenize(input);
            if (tokens.Count == 0) return true;
            if (!AreDelimitersBalanced(tokens)) return false;
            if (HasIncompleteLetExpression(tokens)) return false;
            if (HasIncompleteLambdaExpression(tokens)) return false;
            if (HasIncompleteMacroDefinition(tokens)) return false;
            try { _parser.Parse(input); return true; }
            catch (ParseException ex)
            {
                return ex.ErrorType switch
                {
                    TreeErrorType.UnclosedParen => false,
                    TreeErrorType.MissingLambdaBody => false,
                    TreeErrorType.MissingLetValue => false,
                    TreeErrorType.MissingLetBody => false,
                    TreeErrorType.MissingLetIn => false,
                    TreeErrorType.EmptyExprList when tokens.Count > 0 => false,
                    _ => true
                };
            }
        }
        catch { return true; }
    }

    private bool AreDelimitersBalanced(List<Token> tokens)
    {
        var parenDepth = 0; var bracketDepth = 0;
        foreach (var token in tokens)
        {
            switch (token.Type)
            {
                case TokenType.LParen: parenDepth++; break;
                case TokenType.RParen: parenDepth--; if (parenDepth < 0) return false; break;
                case TokenType.LBracket: bracketDepth++; break;
                case TokenType.RBracket: bracketDepth--; if (bracketDepth < 0) return false; break;
            }
        }
        return parenDepth == 0 && bracketDepth == 0;
    }

    private bool HasIncompleteLetExpression(List<Token> tokens)
    {
        for (int i = 0; i < tokens.Count; i++)
        {
            if (tokens[i].Type == TokenType.Let)
            {
                var hasIn = false; var depth = 0;
                for (int j = i + 1; j < tokens.Count; j++)
                {
                    switch (tokens[j].Type)
                    {
                        case TokenType.LParen or TokenType.LBracket: depth++; break;
                        case TokenType.RParen or TokenType.RBracket: depth--; break;
                        case TokenType.In when depth == 0: hasIn = true; break;
                    }
                }
                if (!hasIn) return true;
            }
        }
        return false;
    }

    private bool HasIncompleteLambdaExpression(List<Token> tokens)
    {
        for (int i = 0; i < tokens.Count; i++)
        {
            if (tokens[i].Type == TokenType.Lambda)
            {
                var hasDot = false; var depth = 0;
                for (int j = i + 1; j < tokens.Count; j++)
                {
                    switch (tokens[j].Type)
                    {
                        case TokenType.LParen or TokenType.LBracket: depth++; break;
                        case TokenType.RParen or TokenType.RBracket: depth--; break;
                        case TokenType.Dot when depth == 0: hasDot = true; goto checkNext;
                    }
                }
            checkNext:
                if (!hasDot) return true;
            }
        }
        return false;
    }

    private bool HasIncompleteMacroDefinition(List<Token> tokens)
    {
        for (int i = 0; i < tokens.Count; i++)
        {
            if (tokens[i].Type == TokenType.Macro)
            {
                var hasArrow = false; var depth = 0;
                for (int j = i + 1; j < tokens.Count; j++)
                {
                    switch (tokens[j].Type)
                    {
                        case TokenType.LParen or TokenType.LBracket: depth++; break;
                        case TokenType.RParen or TokenType.RBracket: depth--; break;
                        case TokenType.FatArrow when depth == 0: hasArrow = true; goto checkNext;
                    }
                }
            checkNext:
                if (!hasArrow) return true;
            }
        }
        return false;
    }
    
    // Save the current environment to a file or to console if path is "console"
    internal async Task<string> SaveFileAsync(string path)
    {
        if (string.IsNullOrWhiteSpace(path))
            return "Error: Please specify a filename. Usage: :save <filename>";

        try
        {
            var lines = new List<string>();

            // Add file header with timestamp and stats
            var timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");
            var definitionCount = _context.Count;
            var infixCount = _parser._infixOperators.Count;
            var macroCount = _parser._macros.Count;

            lines.Add($"# ============================================================================");
            lines.Add($"# Lambda Calculus Environment Export");
            lines.Add($"# Generated: {timestamp}");
            lines.Add($"# Definitions: {definitionCount}, Infix Operators: {infixCount}, Macros: {macroCount}");
            lines.Add($"# ============================================================================");
            lines.Add("");

            // Save variable definitions/assignments
            if (_context.Count > 0)
            {
                lines.Add("# =============================================================================");
                lines.Add("# VARIABLE DEFINITIONS");
                lines.Add("# =============================================================================");
                lines.Add("");

                // Group definitions by type for better organization
                var simpleVars = new List<(string, Expr)>();
                var functionVars = new List<(string, Expr)>();
                var complexVars = new List<(string, Expr)>();

                foreach (var (key, value) in _context.OrderBy(kv => kv.Key))
                {
                    // Categorize by expression type for better file organization
                    if (value.Type == ExprType.Abs)
                        functionVars.Add((key, value));
                    else if (value.Type == ExprType.Var ||
                            (value.Type == ExprType.App && IsSimpleApplication(value)))
                        simpleVars.Add((key, value));
                    else
                        complexVars.Add((key, value));
                }
                
                // Write simple variables first
                if (simpleVars.Count > 0)
                {
                    lines.Add("# Simple definitions and constants");
                    foreach (var (key, value) in simpleVars)
                        lines.Add($"{key} = {FormatWithNumerals(value)}");
                    lines.Add("");
                }

                // Write function definitions
                if (functionVars.Count > 0)
                {
                    lines.Add("# Function definitions");
                    foreach (var (key, value) in functionVars)
                        lines.Add($"{key} = {FormatWithNumerals(value)}");
                    lines.Add("");
                }

                // Write complex expressions
                if (complexVars.Count > 0)
                {
                    lines.Add("# Complex expressions");
                    foreach (var (key, value) in complexVars)
                        lines.Add($"{key} = {FormatWithNumerals(value)}");
                    lines.Add("");
                }
            }

            // Save infix operators first (they need to be defined before use)
            if (_parser._infixOperators.Count > 0)
            {
                lines.Add("# =============================================================================");
                lines.Add("# INFIX OPERATORS");
                lines.Add("# =============================================================================");
                lines.Add("");

                var operators = _parser._infixOperators.Values
                    .OrderByDescending(op => op.Precedence)
                    .ThenBy(op => op.Symbol);

                foreach (var op in operators)
                    lines.Add($":infix {op.Symbol} {op.Precedence} {op.Associativity.ToString().ToLower()}");
                lines.Add("");
            }

            // Save macro definitions (they should be defined before variable assignments that might use them)
            if (_parser._macros.Count > 0)
            {
                lines.Add("# =============================================================================");
                lines.Add("# MACRO DEFINITIONS");
                lines.Add("# =============================================================================");
                lines.Add("");

                foreach (var macro in _parser.ShowMacros())
                {
                    // Use the macro's ToString method which formats it properly
                    lines.Add(macro);
                }
                lines.Add("");
            }

            // Add footer with loading instructions
            lines.Add("");
            lines.Add("# =============================================================================");
            lines.Add("# END OF EXPORT");
            lines.Add("# =============================================================================");
            lines.Add("# To load this environment, use: :load " + Path.GetFileName(path));
            lines.Add("# Note: This will add to your current environment. Use :clear first for a clean state.");

            if (path == "console")
            {
                // Write the lines to the console instead of a file
                foreach (var line in lines)
                    _logger.Log(line);
                return $"Environment displayed in console ({definitionCount} definitions, {infixCount} infix operators, {macroCount} macros)";
            }
            await File.WriteAllLinesAsync(path, lines);

            return $"Environment saved to '{path}' ({definitionCount} definitions, {infixCount} infix operators, {macroCount} macros)";
        }
        catch (Exception ex)
        {
            return $"Error saving to '{path}': {ex.Message}";
        }
    }

}
