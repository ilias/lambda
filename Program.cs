namespace LambdaCalculus;

public enum ExprType : byte { Var, Abs, App }

public record Expr(ExprType Type, string? VarName = null,
                   string? AbsVarName = null, Expr? AbsBody = null,
                   Expr? AppLeft = null, Expr? AppRight = null)
{
    public static int HashCodeCount { get; private set; }
    private int? _hashCode;

    public static Expr Var(string name) => new(ExprType.Var, VarName: name);
    public static Expr Abs(string name, Expr body) => new(ExprType.Abs, AbsVarName: name, AbsBody: body);
    public static Expr App(Expr left, Expr right) => new(ExprType.App, AppLeft: left, AppRight: right);

    public override string ToString() => ToStringLimited(1000);

    private string ToStringLimited(int maxDepth) =>
        maxDepth <= 0 ? "..." : Type switch
        {
            ExprType.Var => VarName!,
            ExprType.Abs => $"λ{AbsVarName}.{AbsBody?.ToStringLimited(maxDepth - 1) ?? "null"}",
            ExprType.App => FormatApplication(maxDepth),
            _ => "?"
        };

    private string FormatApplication(int maxDepth)
    {
        var leftStr = AppLeft!.Type == ExprType.Abs
            ? $"({AppLeft.ToStringLimited(maxDepth - 1)})"
            : AppLeft.ToStringLimited(maxDepth - 1);

        var rightStr = AppRight!.Type is ExprType.App or ExprType.Abs
            ? $"({AppRight.ToStringLimited(maxDepth - 1)})"
            : AppRight.ToStringLimited(maxDepth - 1);

        return $"{leftStr} {rightStr}";
    }

    public virtual bool Equals(Expr? other) => StructuralEquals(other);

    public bool StructuralEquals(Expr? other)
    {
        if (ReferenceEquals(this, other)) return true;
        if (other is null || Type != other.Type) return false;

        var stack = new Stack<(Expr Left, Expr Right)>();
        stack.Push((this, other));

        while (stack.Count > 0)
        {
            var (left, right) = stack.Pop();
            if (left.Type != right.Type) return false;
            if (ReferenceEquals(left, right)) continue;

            switch (left.Type)
            {
                case ExprType.Var when left.VarName != right.VarName: return false;
                case ExprType.Var: break;

                case ExprType.Abs when left.AbsVarName != right.AbsVarName: return false;
                case ExprType.Abs when left.AbsBody is null && right.AbsBody is null: continue;
                case ExprType.Abs when left.AbsBody is null || right.AbsBody is null: return false;
                case ExprType.Abs:
                    stack.Push((left.AbsBody!, right.AbsBody!));
                    break;

                case ExprType.App when AreApplicationsEmpty(left, right): continue;
                case ExprType.App when HasMissingApplicationParts(left, right): return false;
                case ExprType.App:
                    stack.Push((left.AppRight!, right.AppRight!));
                    stack.Push((left.AppLeft!, right.AppLeft!));
                    break;

                default: return false;
            }
        }
        return true;
    }

    private static bool AreApplicationsEmpty(Expr left, Expr right) =>
        left.AppLeft is null && left.AppRight is null &&
        right.AppLeft is null && right.AppRight is null;

    private static bool HasMissingApplicationParts(Expr left, Expr right) =>
        left.AppLeft is null || left.AppRight is null ||
        right.AppLeft is null || right.AppRight is null;
        
    public override int GetHashCode()
    {
        if (_hashCode.HasValue) return _hashCode.Value;

        var stack = new Stack<Expr>();
        stack.Push(this);

        while (stack.Count > 0)
        {
            HashCodeCount++;
            var current = stack.Pop();
            if (current._hashCode.HasValue) continue;

            switch (current.Type)
            {
                case ExprType.Var:
                    current._hashCode = HashCode.Combine(current.Type, current.VarName);
                    break;
                case ExprType.Abs when current.AbsBody is null || current.AbsBody._hashCode.HasValue:
                    current._hashCode = HashCode.Combine(current.Type, current.AbsVarName,
                        current.AbsBody?._hashCode ?? 0);
                    break;
                case ExprType.Abs:
                    stack.Push(current);
                    stack.Push(current.AbsBody!);
                    break;
                case ExprType.App when BothApplicationPartsHaveHashCodes(current):
                    current._hashCode = HashCode.Combine(current.Type,
                        current.AppLeft!._hashCode ?? 0, current.AppRight!._hashCode ?? 0);
                    break;
                case ExprType.App:
                    stack.Push(current);
                    if (current.AppLeft?._hashCode is null) stack.Push(current.AppLeft!);
                    if (current.AppRight?._hashCode is null) stack.Push(current.AppRight!);
                    break;
            }
        }
        return _hashCode ?? 0;
    }

    private static bool BothApplicationPartsHaveHashCodes(Expr expr) =>
        expr.AppLeft?._hashCode is not null && expr.AppRight?._hashCode is not null;
}

public readonly struct ExprEqualityComparer : IEqualityComparer<Expr>
{
    public bool Equals(Expr? x, Expr? y) => (x, y) switch
    {
        (null, null) => true,
        ({ } a, { } b) => a.StructuralEquals(b),
        _ => false
    };

    public int GetHashCode(Expr obj) => obj?.GetHashCode() ?? 0;
}

// Cache key for substitution optimization with pre-computed hash.
public readonly record struct SubstitutionCacheKey(Expr Root, string VarName, Expr Value)
{
    private readonly int _hashCode = unchecked(
        (Root?.GetHashCode() ?? 0) * 397 ^
        (VarName?.GetHashCode() ?? 0) * 31 ^
        (Value?.GetHashCode() ?? 0)
    );
    public override int GetHashCode() => _hashCode;
}

public enum KontinuationType : byte { Empty, Arg, Fun }
public record Kontinuation(KontinuationType Type, Expr? Expression = null, 
    Dictionary<string, Expr>? Environment = null, Expr? Value = null, Kontinuation? Next = null)
{
    public static readonly Kontinuation Empty = new(KontinuationType.Empty);
    
    public static Kontinuation Arg(Expr expr, Dictionary<string, Expr> env, Kontinuation next) => 
        new(KontinuationType.Arg, expr, env, Next: next);
    
    public static Kontinuation Fun(Expr value, Kontinuation next) => 
        new(KontinuationType.Fun, Value: value, Next: next);
}
public record CEKState(Expr Control, Dictionary<string, Expr> Environment, Kontinuation Kontinuation);

public enum TokenType : byte { LParen, RParen, Lambda, Term, Equals, Integer }
public record Token(TokenType Type, int Position, string? Value = null);
public enum TreeErrorType : byte { UnclosedParen, UnopenedParen, MissingLambdaVar, MissingLambdaBody, EmptyExprList, IllegalAssignment }
public class ParseException(TreeErrorType errorType, int position) 
    : Exception($"{errorType} at position {position}")
{
    public TreeErrorType ErrorType { get; } = errorType;
    public int Position { get; } = position;
}

public enum StatementType : byte { Expr, Assignment }
public record Statement(StatementType Type, Expr Expression, string? VarName = null)
{
    public static Statement ExprStatement(Expr expr) =>
        new(StatementType.Expr, expr);

    public static Statement AssignmentStatement(string varName, Expr expr) =>
        new(StatementType.Assignment, expr, varName);

    public override string ToString() => Type == StatementType.Expr
        ? Expression.ToString()
        : $"{VarName} = {Expression}";
}

public static class Parser
{
    public static List<Token> Tokenize(string input)
    {
        if (string.IsNullOrWhiteSpace(input)) return [];

        var result = new List<Token>();
        var currentTerm = new System.Text.StringBuilder();
        var pos = 0;

        for (var i = 0; i < input.Length; i++)
        {
            var ch = input[i];
            pos++;
            if (ch == '#') break; // Comments

            Token? nextToken = ch switch
            {
                '\\' or 'λ' => new Token(TokenType.Lambda, pos),
                '(' => new Token(TokenType.LParen, pos),
                ')' => new Token(TokenType.RParen, pos),
                '=' => new Token(TokenType.Equals, pos),
                '@' => ParseInteger(input, ref i, ref pos),
                _ => null
            };

            if (nextToken is null && !char.IsWhiteSpace(ch) && ch != '.')
            {
                currentTerm.Append(ch);
            }
            else
            {
                if (currentTerm.Length > 0)
                {
                    result.Add(new Token(TokenType.Term, pos - currentTerm.Length, currentTerm.ToString()));
                    currentTerm.Clear();
                }
                if (nextToken is not null)
                    result.Add(nextToken);
            }
        }

        if (currentTerm.Length > 0)
            result.Add(new Token(TokenType.Term, pos - currentTerm.Length + 1, currentTerm.ToString()));

        return result;
    }

    private static Token ParseInteger(string input, ref int i, ref int pos)
    {
        var start = i + 1;
        var startPos = pos;
        while (i + 1 < input.Length && (char.IsDigit(input[i + 1]) || input[i + 1] == ','))
        {
            pos++;
            i++;
        }
        return new Token(TokenType.Integer, startPos, input[start..(i + 1)].Replace(",", ""));
    }

    public static Statement? Parse(string input)
    {
        var tokens = Tokenize(input);
        if (tokens.Count == 0) return null;

        // Assignment: name = expr
        if (tokens.Count > 2 && tokens[0].Type == TokenType.Term && tokens[1].Type == TokenType.Equals)
            return Statement.AssignmentStatement(
                tokens[0].Value!,
                BuildExpressionTree(tokens, 2, tokens.Count - 1));

        // Expression statement
        return Statement.ExprStatement(BuildExpressionTree(tokens, 0, tokens.Count - 1));
    }

    private static Expr BuildExpressionTree(List<Token> tokens, int start, int end)
    {
        var expressions = new List<Expr>();
        for (var i = start; i <= end; i++)
        {
            var token = tokens[i];
            expressions.Add(token.Type switch
            {
                TokenType.LParen => ParseParenthesizedExpr(tokens, ref i, end),
                TokenType.RParen => throw new ParseException(TreeErrorType.UnopenedParen, token.Position),
                TokenType.Lambda => ParseLambdaExpr(tokens, ref i, end),
                TokenType.Term => Expr.Var(token.Value!),
                TokenType.Integer when int.TryParse(token.Value, out int value) => CreateChurchNumeral(value),
                TokenType.Equals => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position),
                _ => throw new ParseException(TreeErrorType.EmptyExprList, token.Position)
            });
        }
        return expressions.Count == 0
            ? throw new ParseException(TreeErrorType.EmptyExprList, tokens.Count > 0 ? tokens[0].Position : 0)
            : expressions.Aggregate(Expr.App);
    }

    private static Expr ParseParenthesizedExpr(List<Token> tokens, ref int i, int end)
    {
        int start = i, nesting = 0;
        for (var j = i + 1; j <= end; j++)
        {
            if (tokens[j].Type == TokenType.LParen) nesting++;
            else if (tokens[j].Type == TokenType.RParen)
            {
                if (nesting == 0)
                {
                    var expr = BuildExpressionTree(tokens, start + 1, j - 1);
                    i = j;
                    return expr;
                }
                nesting--;
            }
        }
        throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
    }

    private static Expr ParseLambdaExpr(List<Token> tokens, ref int i, int end)
    {
        if (i + 2 > end)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i].Position);
        if (tokens[i + 1].Type != TokenType.Term)
            throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[i].Position);
        var varName = tokens[i + 1].Value!;
        var body = BuildExpressionTree(tokens, i + 2, end);
        i = end;
        return Expr.Abs(varName, body);
    }

    private static Expr CreateChurchNumeral(int n) => n < 1
        ? Expr.Abs("f", Expr.Abs("x", Expr.Var("x")))
        : Expr.Abs("f", Expr.Abs("x", GenerateApplicationChain("f", "x", n)));

    private static Expr GenerateApplicationChain(string funcVar, string baseVar, int count) =>
        Enumerable.Range(0, count).Aggregate(Expr.Var(baseVar), (acc, _) => Expr.App(Expr.Var(funcVar), acc));
}

public class Logger
{
    private string _logFile = string.Empty;
    private StreamWriter? _logWriter;
    private readonly SemaphoreSlim _logFileLock = new(1);

    private static readonly Dictionary<string, string> _colors = new(StringComparer.Ordinal)
    {
        ["red"] = "\u001b[31m",
        ["green"] = "\u001b[32m",
        ["yellow"] = "\u001b[33m",
        ["blue"] = "\u001b[34m",
        ["magenta"] = "\u001b[35m",
        ["cyan"] = "\u001b[36m",
        ["white"] = "\u001b[37m",
        ["gray"] = "\u001b[90m",
        ["reset"] = "\u001b[0m"
    };

    public static string Prompt(string txt) => $"{_colors["cyan"]}{txt}{_colors["reset"]} ";
    public string LogStatus => _logFile == string.Empty ? "DISABLED" : _logFile;

    public async Task<string> HandleLogCommandAsync(string arg) => arg switch
    {
        "off" or "" => (_logFile = string.Empty, "Logging is disabled.").Item2,
        "clear" => await ClearLogFileAsync(),
        _ => (_logFile = arg, $"Logging is enabled to '{arg}'").Item2
    };

    public async Task<string> ClearLogFileAsync()
    {
        try
        {
            await CloseLogFileAsync();
            await File.WriteAllTextAsync(_logFile, string.Empty);
            return $"Log file '{_logFile}' cleared.";
        }
        catch (Exception ex)
        {
            return $"Error: clearing log file: {ex.Message}";
        }
    }

    public async Task CloseLogFileAsync()
    {
        if (_logWriter is null) return;
        await _logWriter.DisposeAsync();
        _logWriter = null;
    }

    public static string GetColor(string message) => message switch
    {
        string s when s.StartsWith("Error:") => _colors["red"],
        string s when s.StartsWith("#") => _colors["yellow"], // Comments
        string s when s.StartsWith("->") => _colors["green"], // Results/Assignments
        string s when s.StartsWith("Step") => _colors["yellow"], // Evaluation steps
        string s when s.StartsWith("Time:") => _colors["magenta"], // Timing info
        string s when s.StartsWith("Result ") => _colors["magenta"], // Final result details
        string s when s.Contains("Loading") => _colors["cyan"], // loading files
        string s when s.Contains("Memo clear") => _colors["cyan"], // Cache clear
        string s when s.Contains("Memo put") => _colors["magenta"], // Cache puts
        string s when s.Contains("<<") => _colors["gray"], // reading file lines
        string s when s.Contains(">>") => _colors["green"], // result of reading file lines
        _ => _colors["reset"] // Default
    };

    public static void LogToConsole(string message) =>
        Console.WriteLine(GetColor(message) + message.Replace("\t", _colors["reset"]) + _colors["reset"]);

    public async Task LogAsync(string message, bool toConsole = true)
    {
        if (toConsole) LogToConsole(message);
        if (string.IsNullOrWhiteSpace(_logFile)) return;

        await _logFileLock.WaitAsync();
        try
        {
            _logWriter ??= new StreamWriter(_logFile, append: true, encoding: System.Text.Encoding.UTF8);
            await _logWriter.WriteLineAsync(message);
        }
        catch (Exception ex)
        {
            LogToConsole($"Error: writing to log file: {ex.Message}");
        }
        _logFileLock.Release();
    }

    public void Log(string message, bool toConsole = true) =>
        LogAsync(message, toConsole).GetAwaiter().GetResult();
}

public class Interpreter(Logger logger)
{
    private static int _varCounter = 0; private readonly Dictionary<string, Expr> _context = new(StringComparer.Ordinal);
    private readonly Dictionary<SubstitutionCacheKey, Expr> _substitutionCache = new(8192);
    private readonly Dictionary<Expr, Expr> _evaluationCache = new(8192, new ExprEqualityComparer());
    private readonly Dictionary<Expr, HashSet<string>> _freeVarCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _expressionPool = new(2048, StringComparer.Ordinal);
    private readonly Dictionary<(Expr, string), bool> _containsVarCache = new(2048);
    private readonly Dictionary<Expr, Expr> _normalizationCache = new(4096, new ExprEqualityComparer());
    private readonly Logger _logger = logger;
    private readonly System.Diagnostics.Stopwatch _perfStopwatch = new();
    private long _timeInCacheLookup = 0;
    private long _timeInSubstitution = 0;
    private long _timeInEvaluation = 0;
    private int _normalizeCEKCount = 0; // Count of CEK normalizations to track performance impact
    private int _cacheHits = 0;
    private int _cacheMisses = 0;
    private int _totalIterations = 0;
    private int _iterations = 0;
    private int _substitutionExprCount = 0;

    // Stack-based substitution infrastructure for better performance
    private enum SubstOp { Evaluate, BuildAbs, BuildApp, SubstituteInBody }
    private struct StackEntry(Expr node, SubstOp op, object? extra)
    {
        public Expr Node = node;
        public SubstOp Op = op;
        public object? Extra = extra;
    }

    private int _recursionDepth = 0;
    private int _maxRecursionDepth = 20; // Default, can be set by user    

    private readonly Dictionary<string, Expr> _variableCache = new(1024);

    private bool _showStep = false;

    public async Task<(Expr? exp, string str)> ProcessInputAsync(string input)
    {
        try
        {
            _iterations = 0; // Reset iterations for each input
            if (string.IsNullOrWhiteSpace(input)) return (null, "");
            input = input.TrimEnd('\\');

            // Comments
            if (input.StartsWith('#')) return (null, input.Trim());

            // Handle commands
            if (input.Trim().StartsWith(':'))
                return (null, await HandleCommandAsync(input));

            var statement = Parser.Parse(input);
            if (statement is null) return (null, "");

            // handle assignment before evaluation
            if (statement.Type == StatementType.Assignment)
            {
                _context[statement.VarName!] = statement.Expression;
                return (null, $"-> {statement.VarName} = {statement.Expression}");
            }
            var result = EvaluateCEK(statement.Expression);
            _totalIterations += _iterations; // Update total iterations 

            return (result, $"-> {result}");
        }
        catch (Exception ex)
        {
            return (null, $"Error: {ex.Message}");
        }
    }

    private async Task DisplayOutput((Expr? expr, string str) result, TimeSpan elapsed)
    {
        if (result.expr is not null)
        {
            var number = ExtractChurchNumeralValue(result.expr);
            string? resultName = _context.FirstOrDefault(kv => kv.Value.StructuralEquals(result.expr)).Key;

            if (number is not null || !string.IsNullOrEmpty(resultName))
                await _logger.LogAsync("Result is" +
                    (number is not null ? $" (Church numeral @{number:#,##0})" : "") +
                    (!string.IsNullOrEmpty(resultName) ? $" (named '{resultName}')" : ""));

            var timeInfo = elapsed.TotalSeconds >= 1
                ? $"{elapsed.TotalSeconds:F2} s"
                : $"{elapsed.TotalMilliseconds:F1} ms";
            await _logger.LogAsync($"Time: {timeInfo}, iterations: {_iterations:#,##0}");
        }

        await _logger.LogAsync(result.str);
    }

    public async Task RunInteractiveLoopAsync()
    {
        Console.WriteLine(ShowHelp());
        var currentInput = new System.Text.StringBuilder();

        while (true)
        {
            if (currentInput.Length == 0)
                Console.WriteLine();
            Console.Write(Logger.Prompt(currentInput.Length == 0 ? "lambda> " : "......> "));

            var line = Console.ReadLine();
            if (line is null)
            {
                Console.WriteLine("\nGoodbye!");
                break;
            }
            if (string.IsNullOrWhiteSpace(line)) continue;

            await _logger.LogAsync($"λ> {line}", false);

            // Handle line continuation
            if (line.EndsWith('\\'))
            {
                currentInput.Append(line[..^1]);
                continue;
            }

            // Combine lines
            currentInput.Append(line);
            var input = currentInput.ToString();
            currentInput.Clear();

            var timing = System.Diagnostics.Stopwatch.StartNew();
            var output = await ProcessInputAsync(input);
            timing.Stop();

            if (output.str == "bye")
            {
                await _logger.CloseLogFileAsync();
                Console.WriteLine("Goodbye!");
                break;
            }

            DisplayOutput(output, timing.Elapsed).GetAwaiter().GetResult();

            await Task.Yield();
        }
    }

    private async Task<string> HandleCommandAsync(string input)
    {
        var parts = input.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
        var command = parts[0];
        var arg = parts.Length > 1 ? parts[1].Trim() : "";
        return command switch
        {
            ":log" => await _logger.HandleLogCommandAsync(arg),
            ":load" => await LoadFileAsync(arg),
            ":save" => await SaveFileAsync(arg),
            ":step" => HandleStep(arg),
            ":clear" => ClearEnvironment(),
            ":stats" => ShowStats(),
            ":help" => ShowHelp(),
            ":env" => ShowEnv(),
            ":exit" or ":quit" => "bye",
            ":perf" => ResetPerformanceCounters(),
            ":depth" => HandleRecursionDepth(arg),
            _ => $"Unknown command: {command}"
        };
    }

    private string HandleStep(string arg)
    {
        _showStep = arg == "on";
        return $"Step mode {(_showStep ? "enabled" : "disabled")}";
    }

    private string HandleRecursionDepth(string arg)=>
        string.IsNullOrWhiteSpace(arg)
            ? $"Current recursion depth limit: {_maxRecursionDepth}" 
            : int.TryParse(arg, out int value) && value >= 10 && value <= 10000
                ? $"Recursion depth limit set to {_maxRecursionDepth = value}"
                : "Error: Please provide a number between 10 and 10000.";

    private string MemoClear()
    {
        _substitutionCache.Clear();
        _evaluationCache.Clear();
        _freeVarCache.Clear();
        _expressionPool.Clear();
        _cacheHits = _cacheMisses = 0;
        GC.Collect();
        return "All caches cleared.";
    }

    public async Task<string> LoadFileAsync(string path)
    {
        int lineCount = 0;
        _logger.Log($"Loading commands from '{path}'");
        foreach (var line in await File.ReadAllLinesAsync(path))
        {
            await _logger.LogAsync($"line {lineCount++} <<: {line}");
            var (_, str) = await ProcessInputAsync(line);
            await _logger.LogAsync(str);
        }
        return $"Loaded {path}";
    }

    private async Task<string> SaveFileAsync(string path)
    {
        await File.WriteAllLinesAsync(path, _context.Select(kv => $"{kv.Key} = {kv.Value}"));
        return $"Saved to {path}";
    }

    private string ClearEnvironment()
    {
        _context.Clear();
        MemoClear(); // Reuse cache clearing logic
        _totalIterations = 0;
        return "Environment cleared.";
    }

    private string ShowEnv()
    {
        _logger.Log("# Current environment:");
        foreach (var (key, value) in _context.OrderBy(kv => kv.Key))
            _logger.Log($"  {key} = {value}");
        return $"# Displayed {_context.Count} definitions.";
    }

    private string ResetPerformanceCounters()
    {
        _timeInCacheLookup = 0;
        _timeInSubstitution = 0;
        _timeInEvaluation = 0;
        _substitutionExprCount = 0;
        return "Performance counters reset.";
    }

    private string ShowStats()
    {
        static string PerOfTotal(long value, long total) => total == 0 ? "0.0%" : $"{value * 100.0 / total:F1}%";
        var totalTime = _timeInCacheLookup + _timeInSubstitution + _timeInEvaluation; return $"""
        === Lambda Interpreter Statistics ===
        Environment:              {_context.Count:#,##0} definitions
        CEK normalization:        ({_normalizeCEKCount:#,##0} normalizations)   
        Recursion depth:          {_maxRecursionDepth:#,##0}               
        Memoization:              
          Substitution cache:     {_substitutionCache.Count:#,##0} entries
          Evaluation cache:       {_evaluationCache.Count:#,##0} entries 
          Cache hits/misses:      {_cacheHits:#,##0} / {_cacheMisses:#,##0} ({PerOfTotal(_cacheHits, _cacheHits + _cacheMisses)})
        Performance:
          Total iterations:       {_totalIterations:#,##0}
          Total hash code calls:  {Expr.HashCodeCount:#,##0}
          Cache lookup time:      {_timeInCacheLookup / 10000.0:#,##0.00} ms ({PerOfTotal(_timeInCacheLookup, totalTime)})
          Substitution time:      {_timeInSubstitution / 10000.0:#,##0.00} ms ({PerOfTotal(_timeInSubstitution, totalTime)}) (called {_substitutionExprCount:#,##0} times)
          Evaluation time:        {_timeInEvaluation / 10000.0:#,##0.00} ms ({PerOfTotal(_timeInEvaluation, totalTime)})
          Total measured time:    {totalTime / 10000.0:#,##0.00} ms
        System:
          Unique var counter:     {_varCounter:#,##0}
          Step-by-step mode:      {(_showStep ? "ENABLED" : "DISABLED")}
          Logging:                {_logger.LogStatus}
          Memory usage:           {GC.GetTotalMemory(false) / 1024:#,##0} KB
          GC collections:         Gen0={GC.CollectionCount(0)} Gen1={GC.CollectionCount(1)} Gen2={GC.CollectionCount(2)}
        """;
    }

    private static string ShowHelp() =>
        """
        Lambda Calculus Interpreter Help:

        Expression Syntax:
          x                  - Variable (e.g., myVar)
          \x.expr or λx.expr - Lambda abstraction (e.g., \x.x or λf.λx.f x)
          (expr)             - Grouping (e.g., (\x.x) y)
          expr1 expr2        - Application (e.g., succ 0)
          name = expr        - Assignment (e.g., id = \x.x)
          @123 or @1,234     - Integer literal, replaced by the Church numeral λf.λx.f^n(x)

        Commands: (Prefix with ':')
          :load <filename>       - Load definitions from a file (e.g., :load stdlib.lambda)
          :save <filename>       - Save current environment to a file (e.g., :save myenv.lambda)
          :log (<filename>|off)  - Log output to a file or disable logging (e.g., :log session.log, :log off)
          :log clear             - Clear the current log file (if logging is enabled)
          :step (on|off)         - Toggle step-by-step evaluation logging (e.g., :step on)
          :stats                 - Show performance and environment statistics
          :depth [number]        - Set or show the maximum recursion depth (default: 100, range: 10-10000)
          :perf reset            - Reset performance counters
          :env                   - Show current definitions in the environment
          :clear                 - Clear the environment and caches
          :help                  - Show this help message
          :exit                  - Exit the interpreter

        Other Features:
          Line continuation: Use '\' at the end of a line to continue input on the next line.
          Comments: Lines starting with '#' are ignored.
          Any command line arguments are treated as files with commands to load
        """;

    private void PutEvalCache(int step, Expr expr, Expr result) => _evaluationCache.TryAdd(expr, result);

    private bool GetEvalCache(Expr expr, out Expr result)
    {
        if (expr is not null && _evaluationCache.TryGetValue(expr, out var cachedResult) && cachedResult is not null)
        {
            _cacheHits++;
            result = cachedResult;
            return true;
        }
        _cacheMisses++;
        result = null!;
        return false;
    }

    private Expr EvaluateCEK(Expr expr)
    {
        _perfStopwatch.Restart();

        var environment = new Dictionary<string, Expr>(_context, StringComparer.Ordinal);
        var state = new CEKState(expr, environment, Kontinuation.Empty);
        var stateStack = new Stack<CEKState>();
        stateStack.Push(state);

        int currentStep = 0;
        Expr? finalResult = null;

        while (stateStack.Count > 0)
        {
            _iterations++;
            var (control, env, kont) = stateStack.Pop(); ;

            if (_showStep)
                _logger.Log($"Step {currentStep++}: CEK \tC: {control}, K: {kont.Type}");

            if (GetEvalCache(control, out var cachedResult))
            {
                ApplyContinuation(cachedResult, env, kont, stateStack, ref finalResult);
                continue;
            }

            switch (control.Type)
            {
                case ExprType.Var:
                    var value = env.TryGetValue(control.VarName!, out var envValue) ? envValue : control;
                    ApplyContinuation(value, env, kont, stateStack, ref finalResult);
                    break;
                case ExprType.Abs:
                    ApplyContinuation(control, env, kont, stateStack, ref finalResult);
                    break;
                case ExprType.App:
                    var argKont = Kontinuation.Arg(control.AppRight!, env, kont);
                    stateStack.Push(new CEKState(control.AppLeft!, env, argKont));
                    break;
            }

            if (finalResult != null)
                break;
        }
        _timeInEvaluation += _perfStopwatch.ElapsedTicks;
        if (finalResult != null)
        {
            var result = NormalizeExpression(finalResult);
            PutEvalCache(currentStep, expr, result);
            return result;
        }

        throw new InvalidOperationException("CEK evaluation completed without returning a value");
    }

    private void ApplyContinuation(Expr value, Dictionary<string, Expr> env, Kontinuation kont, Stack<CEKState> stateStack, ref Expr? finalResult)
    {
        switch (kont.Type)
        {
            case KontinuationType.Empty:
                // Final result - no more continuation
                finalResult = value;
                return;

            case KontinuationType.Arg:
                // We have evaluated the function, now evaluate the argument
                var funKont = Kontinuation.Fun(value, kont.Next!);
                stateStack.Push(new CEKState(kont.Expression!, kont.Environment!, funKont));
                break;
            case KontinuationType.Fun:
                // We have evaluated the argument, now apply the function (stored in kont.Value)
                var function = kont.Value!;
                var argument = value;

                if (function.Type == ExprType.Abs)
                {
                    // Beta reduction - substitute argument for parameter in function body
                    var substituted = Substitute(function.AbsBody!, function.AbsVarName!, argument);
                    stateStack.Push(new CEKState(substituted, env, kont.Next!));
                }
                else
                {
                    // Not a function - create application and continue
                    var app = Expr.App(function, argument);
                    stateStack.Push(new CEKState(app, env, kont.Next!));
                }
                break;
        }
    }

    // Alpha conversion to avoid variable capture
    private (string, Expr) AlphaConvert(string varName, Expr body)
    {
        var newVar = $"{varName}_{_varCounter++}";
        var newBody = Substitute(body, varName, Expr.Var(newVar));
        return (newVar, newBody);
    }

    // Expression interning for memory efficiency
    private Expr Intern(Expr expr)
    {
        if (expr.Type != ExprType.Var) return expr;
        var key = $"var:{expr.VarName}";
        return _expressionPool.TryGetValue(key, out var existing)
            ? existing
            : (_expressionPool[key] = expr);
    }

    private HashSet<string> FreeVars(Expr expr, string? skipVar = null)
    {
        if (expr is null) return [];

        if (_freeVarCache.TryGetValue(expr, out var cached))
            return skipVar is null ? cached : [.. cached.Where(v => v != skipVar)];

        var freeVars = new HashSet<string>();
        var boundVars = new HashSet<string>();
        var stack = new Stack<(Expr Expr, bool InScope, string? BoundVar)>();

        stack.Push((expr, true, null));

        while (stack.Count > 0)
        {
            var (current, inScope, boundVar) = stack.Pop();

            if (!inScope && boundVar is not null)
            {
                boundVars.Remove(boundVar);
                continue;
            }

            switch (current.Type)
            {
                case ExprType.Var:
                    if (!boundVars.Contains(current.VarName!) && (skipVar is null || current.VarName != skipVar))
                        freeVars.Add(current.VarName!);
                    break;

                case ExprType.Abs when current.AbsVarName is not null && current.AbsBody is not null:
                    var absVar = current.AbsVarName;
                    boundVars.Add(absVar);
                    stack.Push((current, false, absVar)); // Cleanup entry
                    stack.Push((current.AbsBody, true, null));
                    break;

                case ExprType.App when current.AppLeft is not null && current.AppRight is not null:
                    stack.Push((current.AppRight, true, null));
                    stack.Push((current.AppLeft, true, null));
                    break;
            }
        }
        if (skipVar is null)
            _freeVarCache[expr] = freeVars;

        return freeVars;
    }

    private bool HasContextVariables(Expr? expr)
    {
        if (expr == null) return false;

        return expr.Type switch
        {
            ExprType.Var => _context.ContainsKey(expr.VarName!),
            ExprType.Abs => HasContextVariables(expr.AbsBody),
            ExprType.App => HasContextVariables(expr.AppLeft) || HasContextVariables(expr.AppRight),
            _ => false,
        };
    }

    private Expr? GetSubCache(Expr root, string var, Expr val)
    {
        var cacheKey = new SubstitutionCacheKey(root, var, val);
        var hasResult = _substitutionCache.TryGetValue(cacheKey, out Expr? result);

        _ = hasResult ? _cacheHits++ : _cacheMisses++;
        return result;
    }

    private void PutSubCache(Expr root, string var, Expr val, Expr result)
        => _substitutionCache[new SubstitutionCacheKey(root, var, val)] = result;

    private Expr Substitute(Expr root, string var, Expr val)
    {
        _substitutionExprCount++;
        // Ultra-fast path for variables with aggressive caching
        if (root.Type == ExprType.Var)
        {
            if (root.VarName == var) return val;

            if (_variableCache.TryGetValue(root.VarName!, out var cached)) return cached;

            // Cache all variable lookups for better performance
            var toCache = Intern(root);
            _variableCache[root.VarName!] = toCache;
            return toCache;
        }

        // Fast path for abstractions that shadow the variable
        if (root.Type == ExprType.Abs && root.AbsVarName == var) return root;

        // Ultra-fast application patterns for common Church numeral operations
        if (root.Type == ExprType.App && IsCommonPattern(root, var, val, out var fastResult)) return fastResult;

        // Always use substitution cache for all substitutions
        Expr? cachedResult = GetSubCache(root, var, val);
        if (cachedResult is not null) return cachedResult;

        _perfStopwatch.Restart();

        Expr result;
        if (++_recursionDepth > _maxRecursionDepth)
            result = SubstituteStackBased(root, var, val);
        else
        {
            result = root.Type switch
            {
                // Abstraction with alpha conversion check
                ExprType.Abs when val.Type != ExprType.Var &&
                                  root.AbsVarName != var &&
                                  QuickFreeVarCheck(val, root.AbsVarName!) =>
                    AlphaConvert(root.AbsVarName!, root.AbsBody!) is var (newVar, newBody)
                        ? Expr.Abs(newVar, Substitute(newBody, var, val))
                        : throw new InvalidOperationException("Alpha conversion failed"),

                ExprType.Abs => Expr.Abs(root.AbsVarName!, Substitute(root.AbsBody!, var, val)),

                // Application with parallel processing for large expressions
                ExprType.App when root.AppLeft != null && root.AppRight != null =>
                    CreateOptimizedApplication(root.AppLeft, root.AppRight, var, val),

                _ => throw new InvalidOperationException($"Unknown expression type: {root.Type}")
            };
        }

        _recursionDepth--;
        _timeInSubstitution += _perfStopwatch.ElapsedTicks;

        PutSubCache(root, var, val, result);

        return result;
    }

    private bool IsCommonPattern(Expr app, string var, Expr val, out Expr result)
    {
        result = null!;

        if (app.AppLeft?.Type != ExprType.Var || app.AppRight?.Type != ExprType.Var)
            return false; // Not a simple application

        var (result1, flag) = (app.AppLeft.VarName == var, app.AppRight.VarName == var) switch
        {
            (true, true) => (Expr.App(val, val), true), // Pattern: (var var) -> val val
            (true, false) => (Expr.App(val, app.AppRight), true), // Pattern: (var x) -> val x
            (false, true) => (Expr.App(app.AppLeft, val), true), // Pattern: (f x) where f doesn't contain var and x is var -> f val
            _ => (app, false) // Neither side contains the variable
        };
        result = result1;
        return flag;
    }

    private bool QuickFreeVarCheck(Expr expr, string varName)
    {
        if (_freeVarCache.TryGetValue(expr, out var freeVars))
            return freeVars.Contains(varName);

        // For simple expressions, do direct check
        return expr.Type switch
        {
            ExprType.Var => expr.VarName == varName,
            ExprType.Abs => expr.AbsVarName != varName && QuickFreeVarCheck(expr.AbsBody!, varName),
            _ => FreeVars(expr).Contains(varName),// Fall back to full computation for complex cases
        };
    }

    private Expr CreateOptimizedApplication(Expr left, Expr right, string var, Expr val)
    => (ContainsVariable(left, var), ContainsVariable(right, var)) switch
    {
        (false, false) => Expr.App(left, right),
        (false, true) => Expr.App(left, Substitute(right, var, val)),
        (true, false) => Expr.App(Substitute(left, var, val), right),
        (true, true) => Expr.App(Substitute(left, var, val), Substitute(right, var, val))
    };

    private bool ContainsVariable(Expr expr, string var)
    {
        if (expr.Type == ExprType.Var)
            return expr.VarName == var;

        var key = (expr, var);
        if (_containsVarCache.TryGetValue(key, out var cached))
            return cached;

        var stack = new Stack<Expr>();
        stack.Push(expr);
        while (stack.Count > 0)
        {
            var current = stack.Pop();
            if (current.Type == ExprType.Var)
            {
                if (current.VarName == var)
                {
                    _containsVarCache[key] = true;
                    return true;
                }
            }
            else if (current.Type == ExprType.Abs && current.AbsBody != null)
            {
                stack.Push(current.AbsBody);
            }
            else if (current.Type == ExprType.App)
            {
                if (current.AppRight != null) stack.Push(current.AppRight);
                if (current.AppLeft != null) stack.Push(current.AppLeft);
            }
        }
        _containsVarCache[key] = false;
        return false;
    }

    private Expr SubstituteStackBased(Expr root, string var, Expr val)
    {
        var opStack = new Stack<StackEntry>(64);
        var resultStack = new Stack<Expr>(64);

        var (currentVar, currentVal) = (var, val);

        opStack.Push(new StackEntry(root, SubstOp.Evaluate, null));

        while (opStack.Count > 0)
        {
            var entry = opStack.Pop();
            var (node, op, extra) = (entry.Node, entry.Op, entry.Extra);

            switch (op)
            {
                case SubstOp.Evaluate when node.Type == ExprType.Var:
                    resultStack.Push(node.VarName == currentVar ? currentVal : Intern(node));
                    continue;

                case SubstOp.Evaluate when node.Type == ExprType.Abs && node.AbsVarName == currentVar:
                    // Fast path for shadowed variables
                    resultStack.Push(node);
                    continue;

                case SubstOp.Evaluate:
                    switch (node.Type)
                    {
                        case ExprType.Abs:
                            var (absVarName, absBody) = (node.AbsVarName!, node.AbsBody!);
                            if (currentVal.Type != ExprType.Var && absVarName != currentVar && FreeVars(currentVal).Contains(absVarName))
                            {
                                // Alpha conversion needed - use block scope to avoid conflicts
                                string newVar = absVarName + _varCounter++;

                                // Setup operations in reverse order
                                opStack.Push(new StackEntry(node, SubstOp.BuildAbs, newVar));
                                opStack.Push(new StackEntry(null!, SubstOp.SubstituteInBody, (currentVar, currentVal)));
                                opStack.Push(new StackEntry(absBody, SubstOp.Evaluate, null));

                                // Perform variable renaming
                                (currentVar, currentVal) = (absVarName, Expr.Var(newVar));
                            }
                            else
                            {
                                // No alpha conversion needed
                                opStack.Push(new StackEntry(node, SubstOp.BuildAbs, absVarName));
                                opStack.Push(new StackEntry(absBody, SubstOp.Evaluate, null));
                            }
                            break;

                        case ExprType.App:
                            // Process application - results will be popped in reverse order
                            opStack.Push(new StackEntry(node, SubstOp.BuildApp, null));
                            opStack.Push(new StackEntry(node.AppRight!, SubstOp.Evaluate, null));
                            opStack.Push(new StackEntry(node.AppLeft!, SubstOp.Evaluate, null));
                            break;
                    }
                    break;

                case SubstOp.BuildAbs:
                    var newBody = resultStack.Pop();
                    var varName = (string)extra!;
                    resultStack.Push(Expr.Abs(varName, newBody));
                    break;

                case SubstOp.BuildApp:
                    var right = resultStack.Pop();
                    var left = resultStack.Pop();
                    resultStack.Push(Expr.App(left, right));
                    break;
                case SubstOp.SubstituteInBody:
                    // Restore previous substitution context
                    (currentVar, currentVal) = ((string, Expr))extra!;
                    break;
            }
        }
 
        return resultStack.Pop();
    }

    private Expr NormalizeExpression(Expr expr)
    {
        if (_normalizationCache.TryGetValue(expr, out var cached))
            return cached;
        var visited = new HashSet<Expr>();
        var result = NormalizeWithVisited(expr, visited, 0, 1000); // Max depth of 1000
        _normalizationCache[expr] = result;
        return result;
    }

    private Expr NormalizeWithVisited(Expr expr, HashSet<Expr> visited, int depth, int maxDepth)
    {
        _normalizeCEKCount++;
        if (_normalizationCache.TryGetValue(expr, out var cached))
            return cached;
        // Prevent infinite recursion
        if (depth > maxDepth || visited.Contains(expr))
            return expr;

        visited.Add(expr);
        try
        {
            Expr result = expr.Type switch
            {
                ExprType.Var => expr,
                ExprType.Abs => Expr.Abs(expr.AbsVarName!, NormalizeWithVisited(expr.AbsBody!, visited, depth + 1, maxDepth)),
                ExprType.App => NormalizeApplicationWithVisited(expr, visited, depth, maxDepth),
                _ => expr
            };
            _normalizationCache[expr] = result;
            return result;
        }
        finally
        {
            visited.Remove(expr);
        }
    }

    private Expr NormalizeApplicationWithVisited(Expr app, HashSet<Expr> visited, int depth, int maxDepth)
    {
        if (depth > maxDepth)
            return app;

        var left = NormalizeWithVisited(app.AppLeft!, visited, depth + 1, maxDepth);
        var right = NormalizeWithVisited(app.AppRight!, visited, depth + 1, maxDepth);

        // If left side is a lambda, perform beta reduction
        if (left.Type == ExprType.Abs)
        {
            var substituted = Substitute(left.AbsBody!, left.AbsVarName!, right);
            // Only continue normalizing if we haven't hit recursion limits
            if (depth < maxDepth - 5)
                return NormalizeWithVisited(substituted, visited, depth + 1, maxDepth);
            return substituted;
        }

        return Expr.App(left, right);
    }

    // Returns the integer value of a Church numeral (λf.λx.f^n(x)), or null if not valid.
    private static int? ExtractChurchNumeralValue(Expr expr)
    {
        if (expr is not { Type: ExprType.Abs, AbsVarName: var f, AbsBody: { Type: ExprType.Abs, AbsVarName: var x, AbsBody: var body } })
            return null;
        if (body is { Type: ExprType.Var, VarName: var v } && v == x)
            return 0;
        int n = 0;
        while (body is { Type: ExprType.App, AppLeft: { Type: ExprType.Var, VarName: var fn }, AppRight: var next } && fn == f)
        {
            n++;
            body = next;
        }
        return body is { Type: ExprType.Var, VarName: var v2 } && v2 == x ? n : null;
    }
}

public class Program
{
    public static async Task Main(string[] args)
    {
        Console.OutputEncoding = System.Text.Encoding.UTF8; // Ensure console output supports UTF-8
        Console.InputEncoding = System.Text.Encoding.UTF8; // Ensure console input supports UTF-8

        var interpreter = new Interpreter(logger: new());

        // Load standard library if available
        if (File.Exists("stdlib.lambda"))
            await interpreter.ProcessInputAsync(":load stdlib.lambda");

        int filesCount = 0;
        // Process any command line files before starting interactive mode
        foreach (var filePath in args)
        {
            if (File.Exists(filePath))
            {
                await interpreter.LoadFileAsync(filePath);
                filesCount++;
            }
            else
                Console.WriteLine($"File not found: {filePath}");
        }
        if (filesCount > 0) return;

        await interpreter.RunInteractiveLoopAsync();
    }
}