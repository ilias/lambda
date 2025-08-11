namespace LambdaCalculus;

internal enum SubstOp { Evaluate, BuildAbs, BuildApp, SubstituteInBody }
internal readonly record struct StackEntry(Expr Node, SubstOp Op, object? Extra);

public record CEKState(Expr Control, Dictionary<string, Expr> Environment, Kontinuation Kontinuation);

public partial class Interpreter
{
    private readonly Dictionary<string, Expr> _context = new(StringComparer.Ordinal);
    private readonly Dictionary<SubstitutionCacheKey, Expr> _substitutionCache = new(8192);
    private readonly Dictionary<Expr, Expr> _evaluationCache = new(8192, new ExprEqualityComparer());
    private readonly Dictionary<Expr, HashSet<string>> _freeVarCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _expressionPool = new(2048, StringComparer.Ordinal);
    private readonly Dictionary<(Expr, string), bool> _containsVarCache = new(2048);
    private readonly Dictionary<Expr, Expr> _normalizationCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _variableCache = new(1024);
    private readonly Logger _logger;
    private readonly Statistics _stats;
    private readonly Parser _parser = new();
    private IEvaluator _evaluator; // strategy
    private readonly System.Diagnostics.Stopwatch _perfStopwatch = new();
    private bool _showStep = false;
    private bool _lazyEvaluation = true;
    private bool _prettyPrint = true;
    private int _nativeArithmetic = 0;
    private bool _useNativeArithmetic = true;
    private bool _usedRandom = false;

    public Interpreter(Logger logger, Statistics? stats = null)
    {
        _logger = logger;
        _stats = stats ?? new Statistics();
    _evaluator = new CEKEvaluator(this); // default strategy
    }

    public async Task<(Expr? exp, string str)> ProcessInputAsync(string input)
    {
        try
        {
            _stats.Iterations = 0;
            _usedRandom = false;
            if (string.IsNullOrWhiteSpace(input)) return (null, "");
            input = input.TrimEnd('\\');
            // Comments (whole-line)
            if (input.StartsWith('#')) return (null, input.Trim());

            // Split input into top-level segments by ';' (outside parens/brackets)
            var segments = SplitTopLevelSegments(input);
            if (segments.Count == 0) return (null, "");

            // Fast path: single segment that is a command
            if (segments.Count == 1 && segments[0].TrimStart().StartsWith(':'))
                return (null, await HandleCommandAsync(segments[0]));

            var sb = new System.Text.StringBuilder();
            Expr? lastExpr = null;
            foreach (var seg in segments)
            {
                var trimmed = seg.Trim();
                if (trimmed.Length == 0) continue;
                if (trimmed.StartsWith('#')) continue; // skip comment segment

                if (trimmed.StartsWith(':'))
                {
                    // Command segment
                    var cmdResult = await HandleCommandAsync(trimmed);
                    sb.AppendLine(cmdResult);
                    // If exit/quit, stop processing further segments
                    if (trimmed is ":exit" or ":quit")
                        break;
                    continue;
                }

                // Expression / assignment segment (may itself contain multiple assignments if user grouped without semicolons)
                var statements = _parser.ParseAll(trimmed);
                foreach (var st in statements)
                {
                    if (st.Type == StatementType.Assignment)
                    {
                        var val = _evaluator.Evaluate(st.Expression);
                        _context[st.VarName!] = val;
                        sb.AppendLine($"-> {st.VarName} = {FormatWithNumerals(val)}");
                        lastExpr = null;
                    }
                    else
                    {
                        _logger.Log($"Eval: {FormatWithNumerals(st.Expression)}");
                        if (_showStep) _logger.Log($"Processing: {st}");
                        var res = _evaluator.Evaluate(st.Expression);
                        var norm = NormalizeExpression(res);
                        _stats.TotalIterations += _stats.Iterations;
                        sb.AppendLine($"-> {FormatWithNumerals(norm)}");
                        lastExpr = norm;
                    }
                }
            }

            return (lastExpr, sb.ToString().TrimEnd());
        }
        catch (Exception ex)
        {
            return (null, $"Error: {ex.Message}");
        }
        finally
        {
            _stats.VarCounter = 0; // Reset recursion depth counter
        }
    }

    // Split a raw input line into top-level semicolon-separated segments, ignoring semicolons inside () and []
    private static List<string> SplitTopLevelSegments(string input)
    {
        var segments = new List<string>();
        int paren = 0, bracket = 0; int start = 0;
        for (int i = 0; i < input.Length; i++)
        {
            var c = input[i];
            switch (c)
            {
                case '(': paren++; break;
                case ')': if (paren > 0) paren--; break;
                case '[': bracket++; break;
                case ']': if (bracket > 0) bracket--; break;
                case ';' when paren == 0 && bracket == 0:
                    var seg = input.Substring(start, i - start).Trim();
                    if (seg.Length > 0) segments.Add(seg);
                    start = i + 1;
                    break;
            }
        }
        if (start < input.Length)
        {
            var last = input[start..].Trim();
            if (last.Length > 0) segments.Add(last);
        }
        return segments;
    }

    // Ensure built-in range and range2 functions exist (lazy, stepped ranges)
    public async Task EnsureRangeBuiltinsAsync()
    {
        bool needRange = !_context.ContainsKey("range");
        bool needRange2 = !_context.ContainsKey("range2");
        if (!needRange && !needRange2) return;
        var defs = new List<string>();
        if (needRange)
            defs.Add(":let range = (fix (lambda r. lambda a. lambda b. (if (= a b) (cons a nil) (if (< a b) (cons a (r (succ a) b)) (cons a (r (pred a) b))))))");
        if (needRange2)
            defs.Add(":let range2 = (lambda a. lambda b. lambda c. ((lambda step. (if (= step 0) (cons a nil) ((fix (lambda r. lambda x. (if (if (> step 0) (<= x c) (>= x c)) (cons x (r ((if (> step 0) succ pred) x))) nil))) a))) (- b a)))");
        var joined = string.Join("; ", defs);
        await ProcessInputAsync(joined);
    }

    private async Task DisplayOutput((Expr? expr, string str) result, TimeSpan elapsed)
    {   
        if (result.expr is not null)
        {
            var number = ExtractChurchNumeralValue(result.expr);
            var names = _context.Where(kv => kv.Value.StructuralEquals(result.expr))
                .Select(kv => kv.Key).ToList();

            var resultStr = 
                (number is not null ? $"[Church numeral: {number:#,##0}] " : "") +
                (names.Count > 0 ? $"[defined: {string.Join(", ", names)}]" : "");
            if (resultStr.Length > 2)
                await _logger.LogAsync($"Name: {resultStr}");

            var timeInfo = elapsed.TotalSeconds >= 1
                ? $"{elapsed.TotalSeconds:F2} s"
                : $"{elapsed.TotalMilliseconds:F1} ms";
            await _logger.LogAsync($"Time: {timeInfo}, iterations: {_stats.Iterations:#,##0}");
        }

        await _logger.LogAsync(result.str);
        await _logger.LogAsync($" ");
    }

    // Common input processing for both REPL and file loading
    private async Task ProcessAndDisplayInputAsync(string input)
    {
        var timing = System.Diagnostics.Stopwatch.StartNew();
        var output = await ProcessInputAsync(input);
        timing.Stop();
        await DisplayOutput(output, timing.Elapsed);
    }

    // REPL methods moved to Interpreter.REPL.cs partial

    internal Expr EvaluateCEK(Expr expr, Dictionary<string, Expr>? initialEnv = null)
    {
        _perfStopwatch.Restart();
        var environment = new Dictionary<string, Expr>(initialEnv ?? _context, StringComparer.Ordinal);
        var stateStack = new Stack<CEKState>();
        stateStack.Push(new CEKState(expr, environment, Kontinuation.Empty));
        int currentStep = 0;
        Expr? finalResult = null;
        const int maxIterations = 200000; // Prevent infinite loops

        while (stateStack.Count > 0 && _stats.Iterations < maxIterations)
        {
            _stats.Iterations++;
            var (control, env, kont) = stateStack.Pop();
            if (_showStep)
                _logger.Log($"Step {currentStep++}: CEK \tC: {FormatWithNumerals(control)}, K: {kont.Type}");
            if (GetEvalCache(control, out var cachedResult))
            {
                ApplyContinuation(cachedResult, env, kont, stateStack, ref finalResult);
                continue;
            }

            // Native arithmetic shortcut for Church numerals
            if (control.Type == ExprType.App)
            {
                var nativeResult = TryNativeArithmetic(control, env);
                if (nativeResult != null)
                {
                    ApplyContinuation(nativeResult, env, kont, stateStack, ref finalResult);
                    if (finalResult != null)
                        break;
                    continue;
                }
            }

            switch (control)
            {
                case { Type: ExprType.Var, VarName: var v }:
                    var value = env.TryGetValue(v!, out var envValue) ? envValue : control;
                    ApplyContinuation(value, env, kont, stateStack, ref finalResult);
                    break;
                case { Type: ExprType.Abs }:
                    ApplyContinuation(control, env, kont, stateStack, ref finalResult);
                    break;
                case { Type: ExprType.App, AppLeft: var left, AppRight: var right }:
                    // Special handling for Church conditionals to ensure lazy evaluation of branches
                    if (IsChurchConditional(control, env))
                        HandleChurchConditional(control, env, kont, stateStack);
                    else
                    {
                        var argKont = Kontinuation.Arg(right!, env, kont);
                        stateStack.Push(new CEKState(left!, env, argKont));
                    }
                    break;
                case { Type: ExprType.Thunk }:
                    var forcedValue = Force(control);
                    ApplyContinuation(forcedValue, env, kont, stateStack, ref finalResult);
                    break;
                case { Type: ExprType.YCombinator }:
                    // The Y combinator is a value that can be applied to arguments
                    ApplyContinuation(control, env, kont, stateStack, ref finalResult);
                    break;
            }
            if (finalResult != null)
                break;
        }

        // Check if we hit the iteration limit
        if (_stats.Iterations >= maxIterations)
        {
            throw new InvalidOperationException($"Evaluation exceeded maximum iterations ({maxIterations}). This may indicate an infinite loop, possibly due to undefined variables or non-terminating recursion.");
        }

        _stats.TimeInEvaluation += _perfStopwatch.ElapsedTicks;
        if (finalResult != null)
        {
            // do not cache results if random numbers were used
            if (!_usedRandom)
                PutEvalCache(currentStep, expr, finalResult);
            return finalResult;
        }
        throw new InvalidOperationException("CEK evaluation completed without returning a value");
    }

    // Native arithmetic for Church numerals
    private Expr? TryNativeArithmetic(Expr app, Dictionary<string, Expr> env)
    {
        if (!_useNativeArithmetic)
            return null;

        // Unroll left-associative applications: (((op a) b) c) ...
        var args = new List<Expr>();
        Expr? cur = app;
        while (cur is { Type: ExprType.App, AppLeft: var l, AppRight: var r })
        {
            args.Insert(0, r!);
            cur = l;
        }
        if (cur is not { Type: ExprType.Var, VarName: var opName })
            return null;

        if (args.Count < 1 || args.Count > 2)
            return null; // Only support unary or binary operations

        if (!TryGetChurchInt(args[0], env, out var a))
            return null; // First argument must be a Church numeral
        var b = 0; // Default value for second argument if avaiable
        var isArg2Number = args.Count == 2 && TryGetChurchInt(args[1], env, out b);

        // Only intercept known arithmetic primitives
        int? intResult = (opName, args.Count, isArg2Number) switch
        {
            ("plus" or "+", 2, true) => a + b,
            ("minus" or "-", 2, true) => Math.Max(0, a - b),   
            ("mult" or "*", 2, true) => a * b,
            ("div" or "/", 2, true) => b == 0 ? 0 : a / b,
            ("mod" or "%", 2, true) => b == 0 ? 0 : a % b,
            ("exp" or "pow" or "^", 2, true) => (int)Math.Pow(a, b),
            ("max", 2, true) => Math.Max(a, b),
            ("min", 2, true) => Math.Min(a, b),

            ("succ" or "++", 1, _) => a + 1,
            ("pred" or "--", 1, _) => Math.Max(0, a - 1),
            ("square", 1, _) => a * a,
            ("double", 1, _) => a * 2,
            ("half", 1, _) => a / 2,
            ("sqrt", 1, _) => (int)Math.Sqrt(a),
            ("random", 1, _) => (_usedRandom = true, new Random().Next(0, a + 1)).Item2, // Random number in range [0, a]

            _ => null
        };

        if (intResult is not null)
        {
            _nativeArithmetic++;
            return MakeChurchNumeral(intResult.Value);
        }

        bool? boolResult = (opName, args.Count, isArg2Number) switch
        {
            ("iszero", 1, _) => a == 0,
            ("even", 1, _) => a % 2 == 0,
            ("odd", 1, _) => a % 2 != 0,

            ("lt" or "<", 2, true) => a < b,
            ("leq" or "<=", 2, true) => a <= b,
            ("eq" or "==", 2, true) => a == b,
            ("geq" or ">=", 2, true) => a >= b,
            ("gt" or ">", 2, true) => a > b,
            ("neq" or "!=", 2, true) => a != b,

            _ => null
        };

        if (boolResult is not null)
        {
            _nativeArithmetic++;
            return boolResult.Value
                ? Expr.Abs("f", Expr.Abs("x", Expr.Var("f")))
                : Expr.Abs("f", Expr.Abs("x", Expr.Var("x")));
        }

        return null; // Not a recognized native arithmetic or boolean operation
    }

    // Try to extract a Church numeral as int, resolving variables if needed
    private bool TryGetChurchInt(Expr expr, Dictionary<string, Expr> env, out int value)
    {
        // Resolve variables
        while (expr.Type == ExprType.Var && env.TryGetValue(expr.VarName!, out var v))
            expr = v;
        var n = ExtractChurchNumeralValue(expr);
        if (n != null)
        {
            value = n.Value;
            return true;
        }
        value = 0;
        return false;
    }

    // Build a Church numeral expression for a given int λf.λx.f^n(x)
    private Expr MakeChurchNumeral(int n)
    {
        var f = "f";
        var x = "x";
        Expr body = Expr.Var(x);
        for (int i = 0; i < n; i++)
            body = Expr.App(Expr.Var(f), body);
        return Expr.Abs(f, Expr.Abs(x, body));
    }

    private Expr Intern(Expr expr)
    {
        if (expr.Type != ExprType.Var) return expr;
        var key = $"var:{expr.VarName}";
        if (!_expressionPool.TryGetValue(key, out var existing))
            _expressionPool[key] = existing = expr;
        return existing;
    }

    private bool GetEvalCache(Expr expr, out Expr result)
        => GetFromCache(_evaluationCache, expr, out result);

    // Consolidated cache access pattern
    private bool GetFromCache<TKey, TValue>(Dictionary<TKey, TValue> cache, TKey key, out TValue result) 
        where TKey : notnull
        where TValue : class
    {
        if (key is not null && cache.TryGetValue(key, out var cachedResult) && cachedResult is not null)
        {
            _stats.CacheHits++;
            result = cachedResult;
            return true;
        }
        _stats.CacheMisses++;
        result = null!;
        return false;
    }

    // REPL methods moved to Interpreter.REPL.cs partial

    // Enhanced multi-line input support with intelligent completion detection
    // REPL methods moved to Interpreter.REPL.cs partial
    
    // Intelligent detection of incomplete expressions
    // REPL methods moved to Interpreter.REPL.cs partial
    
    // REPL methods moved to Interpreter.REPL.cs partial
    
    // REPL methods moved to Interpreter.REPL.cs partial
    
    // REPL methods moved to Interpreter.REPL.cs partial
    
    // REPL methods moved to Interpreter.REPL.cs partial

    // REPL methods moved to Interpreter.REPL.cs partial

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
            ":lazy" => HandleLazy(arg),
            ":clear" => ClearEnvironment(),
            ":stats" => ShowStats(),
            ":help" => ShowHelp(),
            ":env" => await ShowEnv(),
            ":memo" => MemoClear(),
            ":exit" or ":quit" => "bye",
            ":depth" => HandleRecursionDepth(arg),
            ":infix" => HandleInfixCommand(arg),
            ":native" => HandleNativeArithmetic(arg),
            ":pretty" => HandlePrettyPrint(arg),
            ":macros" => ShowMacros(),
            ":macro" => HandleMacroDefinition(arg),
            ":multiline" => ShowMultiLineHelp(),
            _ => $"Unknown command: {command}"
        };
    }

    // ...existing code...

    private string HandleNativeArithmetic(string arg)
    {
        if (arg == "show")
            return ShowNativeFunctions();

        _useNativeArithmetic = arg == "on";
        return "Native arithmetic " + (_useNativeArithmetic ? "enabled" : "disabled");
    }

    private string HandlePrettyPrint(string arg)
    {
        _prettyPrint = arg != "off";
        return $"Pretty printing {(_prettyPrint ? "enabled" : "disabled")}";
    }

    private string HandleStep(string arg)
    {
        _showStep = arg == "on";
        return $"Step mode {(_showStep ? "enabled" : "disabled")}";
    }

    private string HandleLazy(string arg)
    {
        _lazyEvaluation = arg != "off";
        return $"Lazy evaluation {(_lazyEvaluation ? "enabled" : "disabled")}";
    }

    private string HandleRecursionDepth(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg))
            return $"Current recursion depth limit: {_stats.MaxRecursionDepth}";

        if (int.TryParse(arg, out int value) && value >= 10 && value <= 10000)
        {
            _stats.MaxRecursionDepth = value;
            return $"Recursion depth limit set to {_stats.MaxRecursionDepth}";
        }

        return "Error: Please provide a number between 10 and 10000.";
    }

    private string HandleInfixCommand(string arg)
    {
        // Allow trailing comments beginning with '#'
        if (!string.IsNullOrWhiteSpace(arg))
        {
            var hash = arg.IndexOf('#');
            if (hash >= 0)
                arg = arg[..hash].TrimEnd();
        }
        if (string.IsNullOrWhiteSpace(arg))
            return ShowInfixOperators();

        var parts = arg.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        
        if (parts.Length != 3)
            return "Usage: :infix <operator> <precedence> <associativity>\nExample: :infix + 6 left";
        
        var symbol = parts[0];
        
        if (!int.TryParse(parts[1], out int precedence))
            return "Error: Precedence must be a number between 1 and 10";
        
        var associativity = parts[2].ToLowerInvariant();
        
        return _parser.DefineInfixOperator(symbol, precedence, associativity);
    }

    private string HandleMacroDefinition(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg))
            return "Usage: :macro (pattern) => transformation\nExample: :macro (when $cond $body) => (if $cond $body unit)";

        try
        {
            // Parse the macro definition from the argument string
            return _parser.ParseAndDefineMacro(arg);
        }
        catch (Exception ex)
        {
            return $"Error defining macro: {ex.Message}";
        }
    }

    // Consolidated formatting method that uses the enhanced Expr.ToString()
    private string FormatWithNumerals(Expr expr) => 
        expr.ToString(_prettyPrint, _prettyPrint ? ExtractChurchNumeralValue : null);

    // Force evaluation of a thunk (lazy value)
    private Expr Force(Expr expr)
    {
        if (expr.Type != ExprType.Thunk || expr.ThunkValue is null)
            return expr;

        if (expr.ThunkValue.IsForced)
            return expr.ThunkValue.ForcedValue!;

        if (expr.ThunkValue.IsBeingForced)
            return expr; // Return the thunk itself to handle recursion.

        if (_showStep)
            _logger.Log($"Step: Forcing thunk: \t{FormatWithNumerals(expr.ThunkValue.Expression)}");

        _perfStopwatch.Restart();
        _stats.ThunkForceCount++;

        expr.ThunkValue.BeginForce();

        // Evaluate the thunk's expression in its captured environment
        var forced = EvaluateCEK(expr.ThunkValue.Expression, expr.ThunkValue.Environment);

        _stats.TimeInForcing += _perfStopwatch.ElapsedTicks;

        // In-place update of the thunk
        expr.ThunkValue.Force(forced);
        return forced;
    }

    private string MemoClear()
    {
        _substitutionCache.Clear();
        _evaluationCache.Clear();
        _freeVarCache.Clear();
        _expressionPool.Clear();
        _stats.CacheHits = _stats.CacheMisses = 0;
        GC.Collect(); // Suggest garbage collection to free memory
        return "All caches cleared.";
    }

    // Save the current environment to a file or to console if path is "console"
    private async Task<string> SaveFileAsync(string path)
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
    
    // Helper method to determine if an application is "simple" for categorization
    private bool IsSimpleApplication(Expr expr)
    {
        if (expr.Type != ExprType.App) return false;
        
        // Consider it simple if it's a direct application of known functions to constants/variables
        var depth = 0;
        var current = expr;
        
        while (current != null && current.Type == ExprType.App && depth < 3)
        {
            current = current.AppLeft;
            depth++;
        }
        
        // Simple if the final left-most element is a variable and depth is reasonable
        return current?.Type == ExprType.Var && depth <= 2;
    }

    private string ClearEnvironment()
    {
        _context.Clear();
        MemoClear(); // Reuse cache clearing logic
        _stats.Reset();
        return "Environment cleared.";
    }

    // ...existing code...

    private void PutEvalCache(int step, Expr expr, Expr result) => _evaluationCache.TryAdd(expr, result);

    private void ApplyContinuation(Expr value, Dictionary<string, Expr> env, Kontinuation kont, Stack<CEKState> stateStack, ref Expr? finalResult)
    {
        switch (kont)
        {
            case { Type: KontinuationType.Empty }:
                // Final result - no more continuation
                finalResult = value;
                return;

            case { Type: KontinuationType.Arg, Expression: var expr, Environment: var kenv, Next: var next }:
                // We have evaluated the function, now evaluate the argument
                var funKont = Kontinuation.Fun(value, next!);

                // In lazy evaluation, wrap the argument in a thunk instead of evaluating it immediately
                if (_lazyEvaluation)
                {
                    var thunk = Expr.Thunk(expr!, kenv!);
                    stateStack.Push(new CEKState(thunk, env, funKont));
                }
                else
                    stateStack.Push(new CEKState(expr!, kenv!, funKont));
                break;

            case { Type: KontinuationType.Fun, Value: var function, Next: var next }:
                // We have evaluated the argument, now apply the function (stored in kont.Value)
                var argument = value;

                var funcToApply = function;
                if (funcToApply!.Type == ExprType.Thunk)
                    funcToApply = Force(funcToApply);

                if (funcToApply.Type == ExprType.Abs)
                {
                    // Beta reduction - substitute argument for parameter in function body
                    var substituted = Substitute(funcToApply.AbsBody!, funcToApply.AbsVarName!, argument);
                    stateStack.Push(new CEKState(substituted, env, next!));
                }
                else if (funcToApply.Type == ExprType.YCombinator)
                {
                    // Y f becomes a thunk for f (Y f) to delay recursion.
                    var yf = Expr.App(Expr.YCombinator(), argument);
                    var recursiveExpr = Expr.App(argument, yf);
                    var thunkExpr = Expr.Thunk(recursiveExpr, env);
                    ApplyContinuation(thunkExpr, env, next!, stateStack, ref finalResult);
                }
                else
                {
                    // Not a function - create application and continue with next continuation
                    // This prevents infinite loops with undefined variables
                    var app = Expr.App(funcToApply, argument);
                    ApplyContinuation(app, env, next!, stateStack, ref finalResult);
                }
                break;

            case { Type: KontinuationType.Conditional, ThenBranch: var thenBranch, ElseBranch: var elseBranch, Environment: var condEnv, Next: var next }:
                // Evaluate Church boolean condition using centralized boolean pattern (Expr.TryExtractBoolean)
                // true = λa.λb.a, false = λa.λb.b
                var isTrue = Expr.TryExtractBoolean(value, out var boolVal) && boolVal;
                stateStack.Push(new CEKState(isTrue ? thenBranch! : elseBranch!, condEnv!, next!));
                break;
        }
    }

    // Alpha conversion to avoid variable capture
    private (string, Expr) AlphaConvert(string varName, Expr body)
    {
        var newVar = $"{varName}_{_stats.VarCounter++}";
        var newBody = Substitute(body, varName, Expr.Var(newVar));
        return (newVar, newBody);
    }

    // Expression interning for memory efficiency
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

                case ExprType.Thunk when current.ThunkValue is not null:
                    // For thunks, analyze the expression inside
                    stack.Push((current.ThunkValue.Expression, true, null));
                    break;

                case ExprType.YCombinator:
                    // Y combinator has no free variables
                    break;
            }
        }
        if (skipVar is null)
            _freeVarCache[expr] = freeVars;

        return freeVars;
    }

    private Expr? GetSubCache(Expr root, string var, Expr val)
    {
        var cacheKey = new SubstitutionCacheKey(root, var, val);
        return GetFromCache(_substitutionCache, cacheKey, out var result) ? result : null;
    }

    private void PutSubCache(Expr root, string var, Expr val, Expr result)
        => _substitutionCache[new SubstitutionCacheKey(root, var, val)] = result;

    private Expr Substitute(Expr root, string var, Expr val)
    {
        // Fast-path: If substituting a variable with itself, return the original expression
        if (_showStep)
            _logger.Log($"Step: Substitute \t{var} := {FormatWithNumerals(val)} in {FormatWithNumerals(root)}");

        if (val.Type == ExprType.Var && val.VarName == var)
            return root;

        // Fast-path: If the variable does not occur free in the expression, return the original
        if (!ContainsVariable(root, var))
            return root;

        // Always use substitution cache for all substitutions (move to top)
        Expr? cachedResult = GetSubCache(root, var, val);
        if (cachedResult is not null) return cachedResult;

        _stats.SubstitutionExprCount++;

        // Ultra-fast path for variables with aggressive caching
        if (root.Type == ExprType.Var)
        {
            if (_showStep && root.VarName == var)
                _logger.Log($"Step: Substitute variable \t{var} with {FormatWithNumerals(val)}");
            if (root.VarName == var) return val;

            if (_expressionPool.TryGetValue(root.VarName!, out var cached)) return cached;

            // Cache all variable lookups for better performance
            var toCache = Intern(root);
            _expressionPool[root.VarName!] = toCache;
            return toCache;
        }

        // Fast path for abstractions that shadow the variable
        if (root.Type == ExprType.Abs && root.AbsVarName == var) return root;
        
        // Handle thunks
        if (root.Type == ExprType.Thunk && root.ThunkValue is not null)
        {
            if (_showStep)
                _logger.Log($"Step: Substitute in thunk: \t{FormatWithNumerals(root.ThunkValue.Expression)}");
            var substitutedExpr = Substitute(root.ThunkValue.Expression, var, val);
            var newThunk = new Thunk(substitutedExpr, root.ThunkValue.Environment);
            if (root.ThunkValue.IsForced)
                newThunk.Force(root.ThunkValue.ForcedValue!);
            return root with { ThunkValue = newThunk };
        }

        // Ultra-fast application patterns for common Church numeral operations
        if (root.Type == ExprType.App && IsCommonPattern(root, var, val, out var fastResult))
            return fastResult;

        _perfStopwatch.Restart();

        Expr result;
        if (++_stats.VarCounter > _stats.MaxRecursionDepth)
            result = SubstituteStackBased(root, var, val);
        else
        {
            result = root.Type switch
            {
                ExprType.Abs when root.AbsVarName != var && QuickFreeVarCheck(val, root.AbsVarName!) =>
                    AlphaConvert(root.AbsVarName!, root.AbsBody!) is var (newVar, newBody)
                        ? Expr.Abs(newVar, Substitute(newBody, var, val))
                        : throw new InvalidOperationException("Alpha conversion failed"),

                ExprType.Abs => Expr.Abs(root.AbsVarName!, Substitute(root.AbsBody!, var, val)),

                ExprType.App when root.AppLeft != null && root.AppRight != null =>
                    CreateOptimizedApplication(root.AppLeft, root.AppRight, var, val),

                ExprType.YCombinator => root, // Y combinator doesn't contain variables

                _ => throw new InvalidOperationException($"Unknown expression type: {root.Type}")
            };
        }

        _stats.VarCounter--;
        _stats.TimeInSubstitution += _perfStopwatch.ElapsedTicks;

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
            ExprType.Thunk => expr.ThunkValue is not null && QuickFreeVarCheck(expr.ThunkValue.Expression, varName),
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
            switch (current.Type)
            {
                case ExprType.Var:
                    if (current.VarName == var)
                    {
                        _containsVarCache[key] = true;
                        return true;
                    }
                    break;

                case ExprType.Abs when current.AbsBody != null:
                    stack.Push(current.AbsBody);
                    break;

                case ExprType.App:
                    if (current.AppRight != null) stack.Push(current.AppRight);
                    if (current.AppLeft != null) stack.Push(current.AppLeft);
                    break;

                case ExprType.Thunk when current.ThunkValue != null:
                    stack.Push(current.ThunkValue.Expression);
                    break;

                case ExprType.YCombinator:
                    // Y combinator has no variables
                    break;
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
                    if (_showStep && node.VarName == currentVar)
                        _logger.Log($"Step: Substitute variable \t{currentVar} with {FormatWithNumerals(currentVal)}");
                    resultStack.Push(node.VarName == currentVar ? currentVal : Intern(node));
                    continue;

                case SubstOp.Evaluate when node.Type == ExprType.Abs && node.AbsVarName == currentVar:
                    // Fast path for shadowed variables
                    resultStack.Push(node);
                    continue;

                case SubstOp.Evaluate when node.Type == ExprType.Thunk:
                    // Handle thunk substitution
                    if (node.ThunkValue is not null)
                    {
                        if (_showStep)
                            _logger.Log($"Step: Substitute in thunk: \t{FormatWithNumerals(node.ThunkValue.Expression)}");
                        var substitutedExpr = Substitute(node.ThunkValue.Expression, currentVar, currentVal);
                        var newThunk = new Thunk(substitutedExpr, node.ThunkValue.Environment);
                        if (node.ThunkValue.IsForced)
                            newThunk.Force(node.ThunkValue.ForcedValue!);
                        resultStack.Push(node with { ThunkValue = newThunk });
                    }
                    else
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
                                string newVar = absVarName + _stats.VarCounter++;

                                // Setup operations in reverse order
                                opStack.Push(new StackEntry(node, SubstOp.BuildAbs, newVar));
                                opStack.Push(new StackEntry(null!, SubstOp.SubstituteInBody, (currentVar, currentVal)));
                                opStack.Push(new StackEntry(absBody, SubstOp.Evaluate, null));

                                // Perform variable renaming
                                (currentVar, currentVal) = (absVarName, Expr.Var(newVar));
                                if (_showStep)
                                    _logger.Log($"Step: Alpha-convert \t{absVarName} to {newVar}");
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

                        case ExprType.YCombinator:
                            // Y combinator has no variables, so substitution returns itself
                            resultStack.Push(node);
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
        _stats.NormalizeCEKCount++;
        if (_normalizationCache.TryGetValue(expr, out var cached))
            return cached;
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
                ExprType.Thunk => Force(expr), // Force thunks during normalization
                ExprType.YCombinator => expr, // Y combinator itself is normalized, but applications should be handled in NormalizeApplicationWithVisited
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
    public static int? ExtractChurchNumeralValue(Expr expr)
    {
        // Force evaluation if it's a thunk
        if (expr.Type == ExprType.Thunk)
            expr = expr.ThunkValue?.IsForced == true ? expr.ThunkValue.ForcedValue! : expr;

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

    // Detect if this is a Church conditional (if p a b) 
    private bool IsChurchConditional(Expr expr, Dictionary<string, Expr> env)
    {
        // Check if this is a fully applied Church conditional: App(App(App(if, condition), then_branch), else_branch)
        if (expr is not { Type: ExprType.App, AppLeft: { Type: ExprType.App, AppLeft: { Type: ExprType.App, AppLeft: var ifExpr } } })
            return false;

        // Check if the leftmost expression resolves to the 'if' function
        if (ifExpr == null) return false;
        var resolvedIf = ResolveVariable(ifExpr, env);
        return IsIfFunction(resolvedIf);
    }

    // Check if an expression is the Church 'if' function: λp.λa.λb.p a b
    private static bool IsIfFunction(Expr expr) =>
        expr is
        {
            Type: ExprType.Abs,
            AbsVarName: var p,
            AbsBody:
            {
                Type: ExprType.Abs,
                AbsVarName: var a,
                AbsBody:
                {
                    Type: ExprType.Abs,
                    AbsVarName: var b,
                    AbsBody:
                    {
                        Type: ExprType.App,
                        AppLeft:
                        {
                            Type: ExprType.App,
                            AppLeft: { Type: ExprType.Var, VarName: var p2 },
                            AppRight: { Type: ExprType.Var, VarName: var a2 }
                        },
                        AppRight: { Type: ExprType.Var, VarName: var b2 }
                    }
                }
            }
        } && p == p2 && a == a2 && b == b2;

    // (Removed IsChurchTrue duplicate – use Expr.TryExtractBoolean instead for centralized pattern logic.)

    // Handle Church conditional with lazy evaluation of branches
    private void HandleChurchConditional(Expr control, Dictionary<string, Expr> env, Kontinuation kont, Stack<CEKState> stateStack)
    {
        // Extract: App(App(App(if, condition), then_branch), else_branch)
        var outerApp = control; // App(App(App(if, condition), then_branch), else_branch)
        var elseExpr = outerApp.AppRight!;
        
        var middleApp = outerApp.AppLeft!; // App(App(if, condition), then_branch)
        var thenExpr = middleApp.AppRight!;
        
        var innerApp = middleApp.AppLeft!; // App(if, condition)
        var conditionExpr = innerApp.AppRight!;
        
        // Evaluate the condition first
        var conditionalKont = Kontinuation.Conditional(thenExpr, elseExpr, env, kont);
        stateStack.Push(new CEKState(conditionExpr, env, conditionalKont));
    }

    // Resolve a variable to its value in the environment
    private Expr ResolveVariable(Expr expr, Dictionary<string, Expr> env)
    {
        if (expr is { Type: ExprType.Var, VarName: var varName } && varName != null && env.TryGetValue(varName, out var value))
            return value;
        return expr;
    }
}
