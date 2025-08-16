namespace LambdaCalculus;

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
    public readonly Statistics _stats;
    public readonly Parser _parser;
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
        _parser = new Parser(logger, this); // Pass logger to parser
        _evaluator = new CEKEvaluator(this); // default strategy
        RegisterNativeFunctions();
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

    private async Task<string> HandleCommandAsync(string input)
    {
        var parts = input.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
        var command = parts[0];
        var arg = parts.Length > 1 ? parts[1].Trim() : "";
        return command switch
        {
            ":test" when arg.Equals("clear", StringComparison.OrdinalIgnoreCase) => TestClear(),
            ":test" when arg.Equals("result", StringComparison.OrdinalIgnoreCase) => TestResult(),
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
    

    // Consolidated formatting method that uses the enhanced Expr.ToString()
    public string FormatWithNumerals(Expr expr) => 
        expr.ToString(_prettyPrint, _prettyPrint ? new System.Func<Expr,int?>(ExtractChurchNumeralValue) : null);

    

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

    private string TestClear()
    {
        _stats.StructEqCalls = 0;
        _stats.StructEqSuccesses = 0;
        return "Test counters cleared (structural equality).";
    }

    private string TestResult()
    {
        return $"Test results: structural equality calls={_stats.StructEqCalls}, successes={_stats.StructEqSuccesses}, success rate={( _stats.StructEqCalls==0 ? 0 : (100.0*_stats.StructEqSuccesses/_stats.StructEqCalls)):F1}%";
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
                ExprType.Var =>
                    // Attempt to inline top-level combinator / function definitions so that
                    // normalization can perform beta-reduction across variable references.
                    // This lets (K 42 99) normalize to 42 instead of remaining as K 42 99
                    // (previously a Var prevented seeing the underlying λ abstraction).
                    // Only inline simple Abs bodies to avoid expanding large data accidentally.
                    (expr.VarName is not null
                        && _context.TryGetValue(expr.VarName, out var bound)
                        && bound.Type == ExprType.Abs)
                        ? bound
                        : expr,
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

    
}
