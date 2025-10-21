namespace LambdaCalculus;

public partial class Interpreter
{
    /// <summary>Runs the interactive REPL loop reading from the console.</summary>
    public async Task RunInteractiveLoopAsync()
        => await InputLoopAsync(async input => await ProcessAndDisplayInputAsync(input!));

    // Helper method to safely load a file if it exists
    /// <summary>Loads a file if it exists, returning a status string.</summary>
    public async Task<string> LoadFileIfExistsAsync(string path)
    {
        if (!File.Exists(path))
            return $"File not found: {path}";
        try { return await LoadFileAsync(path); }
        catch (Exception ex) { return $"Error loading {path}: {ex.Message}"; }
    }

    /// <summary>
    /// Loads a .lambda file, processing commands with multiline / continuation support.
    /// Emits PROGRESS::NN lines (5% increments) and a final load summary for streaming UIs.
    /// </summary>
    public async Task<string> LoadFileAsync(string path)
    {
        _lastLoadedFile = path; // store for :reload
        int lineCount = 1;
        var sw = System.Diagnostics.Stopwatch.StartNew();
        _logger.Log($"Loading commands from '{path}'");
        var lines = await File.ReadAllLinesAsync(path);
        var total = lines.Length;
        var currentInput = new System.Text.StringBuilder();
        int lastProgress = -1;
        _currentSourceFile = path;
        _isLoadingFile = true;
        foreach (var line in lines)
        {
            _currentSourceLine = lineCount + 1; // 1-based line number
            // Inline doc directive: lines starting with '## name: text' capture doc without affecting evaluation
            var trimmed = line.TrimEnd();
            if (trimmed.StartsWith("## "))
            {
                // Format: ## symbol: doc text
                var payload = trimmed[3..].Trim();
                var colon = payload.IndexOf(':');
                if (colon > 0)
                {
                    var name = payload[..colon].Trim();
                    var text = payload[(colon + 1)..].Trim();
                    _docs[name] = text;
                    lineCount++;
                    continue;
                }
            }
            // Also capture minimal module export/hide pseudo-directives when loading modules
            if (_isLoadingFile)
            {
                var trimmed2 = line.Trim();
                if (trimmed2.StartsWith(":module export "))
                {
                    // :module export {a,b,c}
                    var inner = trimmed2[":module export ".Length..].Trim();
                    if (inner.StartsWith("{") && inner.EndsWith("}"))
                    {
                        var items = inner[1..^1].Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
                        var set = new HashSet<string>(items, StringComparer.Ordinal);
                        var exp = _modules.GetValueOrDefault("__exports__") ?? new ModuleInfo { Alias = "__exports__", SourcePath = _currentSourceFile ?? "" };
                        exp.ExportOnly = set;
                        _modules["__exports__"] = exp;
                        lineCount++;
                        continue;
                    }
                }
                if (trimmed2.StartsWith(":module hide "))
                {
                    // :module hide {a,b}
                    var inner = trimmed2[":module hide ".Length..].Trim();
                    if (inner.StartsWith("{") && inner.EndsWith("}"))
                    {
                        var items = inner[1..^1].Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
                        var mod = _modules.GetValueOrDefault("__exports__") ?? new ModuleInfo { Alias = "__exports__", SourcePath = _currentSourceFile ?? "" };
                        foreach (var it in items) mod.Hidden.Add(it);
                        _modules["__exports__"] = mod;
                        lineCount++;
                        continue;
                    }
                }
            }
            await ProcessLineWithContinuation(line, currentInput, ProcessAndDisplayInputAsync, true, lineCount++);
            var pct = (int)((long)lineCount * 100 / total);
            if (pct != lastProgress && (pct == 100 || pct - lastProgress >= 5)) // every 5% + final
            {
                _logger.Log($"PROGRESS::{pct}");
                lastProgress = pct;
            }
        }
        _currentSourceFile = null;
        _currentSourceLine = null;
        _isLoadingFile = false;
        if (currentInput.Length > 0)
            await ProcessAndDisplayInputAsync(currentInput.ToString());
        if (lastProgress < 100)
            _logger.Log("PROGRESS::100");
        sw.Stop();
        _logger.Log($"Loaded file '{path}' lines={total} time={sw.Elapsed.TotalMilliseconds:F1} ms");
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

        var lines = new List<string>();

        void AddTitle(string title)
        {
            lines.Add("# =============================================================================");
            lines.Add($"# {title}");
            lines.Add("# =============================================================================");
            lines.Add("");
        }

        try
        {
            // Add file header with timestamp and stats
            var timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");
            var definitionCount = _contextUnevaluated.Count;
            var infixCount = _parser._infixOperators.Count;
            var macroCount = _parser._macros.Count;
            var nativeCount = _nativeFunctions.Count;
            var totals = $"# Definitions: {definitionCount}, Infix Operators: {infixCount}, Macros: {macroCount}, Native: {nativeCount}";

            lines.Add($"# ============================================================================");
            lines.Add($"# Lambda Calculus Environment Export");
            lines.Add($"# Generated: {timestamp}");
            lines.Add(totals);
            lines.Add($"# ============================================================================");
            lines.Add("");

            List<string> DisplayFuncDefs(string title, List<(string, Expr)> vars)
            {
                var result = new List<string>() { title };
                result.AddRange([.. vars.OrderBy(v => v.Item1)
                                .Select(v => (name: v.Item1, expr: v.Item2, isNative: IsNativeFunction(v.Item1)))
                                .Select(v => $"  {v.name} = {FormatWithNumerals(v.expr)}{v.isNative}")]);
                result.Add("");
                return result;
            }

            string IsNativeFunction(string key) => _nativeFunctions.ContainsKey(key) ? " # native " : "";

            // Save variable definitions/assignments
            if (_contextUnevaluated.Count > 0)
            {
                AddTitle("VARIABLE DEFINITIONS");

                // Group definitions by type for better organization
                var simpleVars = new List<(string, Expr)>();
                var functionVars = new List<(string, Expr)>();
                var complexVars = new List<(string, Expr)>();

                foreach (var (key, value) in _contextUnevaluated.OrderBy(kv => kv.Key))
                {
                    switch (value.Type)
                    {
                        case ExprType.Abs:
                            functionVars.Add((key, value));
                            break;
                        case ExprType.Var:
                        case ExprType.App when IsSimpleApplication(value):
                            simpleVars.Add((key, value));
                            break;
                        default:
                            complexVars.Add((key, value));
                            break;
                    }
                }

                if (simpleVars.Count > 0)
                    lines.AddRange(DisplayFuncDefs("# Simple definitions and constants", simpleVars));

                if (functionVars.Count > 0)
                    lines.AddRange(DisplayFuncDefs("# Function definitions", functionVars));

                if (complexVars.Count > 0)
                    lines.AddRange(DisplayFuncDefs("# Complex expressions", complexVars));
            }

            // Save infix operators first (they need to be defined before use)
            if (_parser._infixOperators.Count > 0)
            {
                AddTitle("INFIX OPERATORS");

                var operators = _parser._infixOperators.Values
                    .OrderByDescending(op => op.Precedence)
                    .ThenBy(op => op.Symbol);

                foreach (var op in operators)
                    lines.Add($"  :infix {op.Symbol} {op.Precedence} {op.Associativity.ToString().ToLower()} {IsNativeFunction(op.Symbol)}");
                lines.Add("");
            }

            // Save macro definitions (they should be defined before variable assignments that might use them)
            if (_parser._macros.Count > 0)
            {
                AddTitle("MACROS");
                foreach (var macro in _parser.ShowMacros())
                    lines.Add(macro);
                lines.Add("");
            }

            // Save native functions (detailed descriptors like :env native)
            if (_nativeFunctions.Count > 0)
            {
                AddTitle("NATIVE FUNCTIONS");
                int totalWithAliases;
                var nativeLines = BuildNativeDescriptorLines(out totalWithAliases);
                lines.AddRange(nativeLines);
                lines.Add($"  # Total native names (incl aliases): {totalWithAliases}");
                lines.Add("");
            }

            if (path == "console")
            {
                // Write the lines to the console instead of a file
                foreach (var line in lines)
                    _logger.Log(line);
                return totals;
            }

            // Add footer with loading instructions
            AddTitle("END OF EXPORT");
            lines.Add("# To load this environment, use: :load " + Path.GetFileName(path));
            lines.Add("# Note: This will add to your current environment. Use :clear first for a clean state.");

            await File.WriteAllLinesAsync(path, lines);

            return $"Environment saved to '{path}' {totals}";
        }
        catch (Exception ex)
        {
            return $"Error saving to '{path}': {ex.Message}";
        }
    }

}
