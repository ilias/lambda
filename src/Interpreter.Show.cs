namespace LambdaCalculus;

public partial class Interpreter
{
    private async Task<string> ShowEnv(string filter)
    {
        var f = (filter ?? string.Empty).Trim().ToLowerInvariant();
        if (string.IsNullOrEmpty(f) || f == "all")
        {
            // Existing export already prints definitions, macros, infix, natives meta
            return await SaveFileAsync("console");
        }

        switch (f)
        {
            case "defs":
                return ShowDefsOnly();
            case "macros":
                return ShowMacrosOnly();
            case "infix":
                return ShowInfixOnly();
            case "native":
                return ShowNativeOnly();
            default:
                return $"Unknown env filter: {filter}. Use :env [defs|macros|infix|native|all]";
        }
    }

    private string ShowDefsOnly()
    {
        // Reuse save logic but limit to definitions section only
        // We'll directly construct similar output subset
        var lines = new List<string>();
        lines.Add("# =============================================================================");
        lines.Add("# VARIABLE DEFINITIONS");
        lines.Add("# =============================================================================\n");
        if (_contextUnevaluated.Count == 0)
        {
            lines.Add("(none)");
        }
        else
        {
            foreach (var kv in _contextUnevaluated.OrderBy(k => k.Key))
            {
                lines.Add($"  {kv.Key} = {FormatWithNumerals(kv.Value)}");
            }
        }
        foreach (var l in lines) _logger.Log(l);
        return $"# Displayed {_contextUnevaluated.Count} definition(s).";
    }

    private string ShowMacrosOnly()
    {
        var macros = _parser.ShowMacros();
        if (macros.Count == 0) return "No macros defined";
        _logger.Log("\n# MACROS\n");
        foreach (var macro in macros) _logger.Log(macro);
        return $"# Displayed {_parser._macros.Count} macro group(s).";
    }

    private string ShowInfixOnly() => ShowInfixOperators();

    private string ShowNativeOnly()
    {
        if (_nativeFunctions.Count == 0) return "No native functions registered.";
        _logger.Log("\n# NATIVE FUNCTIONS\n");
        foreach (var kv in _nativeFunctions.OrderBy(k => k.Key))
            _logger.Log($"  {kv.Key}");
        return $"# Displayed {_nativeFunctions.Count} native function(s).";
    }

    public string ShowInfixOperators()
    {
        if (_parser._infixOperators.Count == 0)
            return "# No infix operators defined";

        var operators = _parser._infixOperators.Values
            .OrderByDescending(op => op.Precedence)
            .ThenBy(op => op.Symbol);

        _logger.Log("\n");
        foreach (var op in operators)
            _logger.Log($"  :infix {op.Symbol} {op.Precedence} {op.Associativity.ToString().ToLower()}");
        return $"# Displayed {_parser._infixOperators.Count} infix operators.";
    }

    private string ShowStats()
    {
        static string PerOfTotal(long value, long total) => total == 0 ? "0.0%" : $"{value * 100.0 / total:F1}%";
        var totalTime = _stats.TimeInCacheLookup + _stats.TimeInSubstitution + _stats.TimeInEvaluation + _stats.TimeInForcing;
        var process = System.Diagnostics.Process.GetCurrentProcess();
        var memUsage = GC.GetTotalMemory(false) / 1024.0;
        var peakMem = process.PeakWorkingSet64 / 1024.0;
        var threads = process.Threads.Count;
        var startTime = process.StartTime;
        var upTime = DateTime.Now - startTime;
        var gen0 = GC.CollectionCount(0);
        var gen1 = GC.CollectionCount(1);
        var gen2 = GC.CollectionCount(2);
        var evalMode = _lazyEvaluation ? "Lazy" : "Eager";
        var cacheHitRate = PerOfTotal(_stats.CacheHits, _stats.CacheHits + _stats.CacheMisses);
        var cacheStats = $"Subst: {_substitutionCache.Count}, Eval: {_evaluationCache.Count}, FreeVar: {_freeVarCache.Count}, Var: {_expressionPool.Count}, ContainsVar: {_containsVarCache.Count}, Norm: {_normalizationCache.Count}";

        foreach (var (macro, count) in _stats.MacroUsage.OrderByDescending(kvp => kvp.Value))
        {
            _logger.Log($"Macro '{macro}': {count:#,##0} uses");
        }
        foreach (var (native, count) in _stats.NativeUsage.OrderByDescending(kvp => kvp.Value))
        {
            _logger.Log($"Native '{native}': {count:#,##0} uses");
        }

        return $"""
        === Lambda Interpreter Statistics ===
        
        -- Environment --
        Definitions:              {_context.Count:#,##0}, infix operators: {_parser._infixOperators.Count:#,##0}, native arithmetic: {(_useNativeArithmetic ? "ENABLED" : "DISABLED")}
        Unique expressions:       {_expressionPool.Count:#,##0}
        Unique var counter:       {_stats.VarCounter:#,##0}
        Pretty printing:          {(_prettyPrint ? "ENABLED" : "DISABLED")}
        
        -- Evaluation --
        Mode:                     {evalMode}
        Recursion depth limit:    {_stats.MaxRecursionDepth:#,##0}, max iterations: {200_000:#,##0}
        Step-by-step:             {(_showStep ? "ENABLED" : "DISABLED")}
        Normalizations:           {_stats.NormalizeCEKCount:#,##0}
        Thunks forced:            {_stats.ThunkForceCount:#,##0}
        Total iterations:         {_stats.TotalIterations:#,##0}
        Hash code calls:          {Expr.HashCodeCount:#,##0}
        Native arithmetic:        {(_useNativeArithmetic ? "ENABLED" : "DISABLED")}, {_nativeArithmetic:#,##0} calls
        Structural equality:      {_stats.StructEqCalls:#,##0} calls, {_stats.StructEqSuccesses:#,##0} successes ({(_stats.StructEqCalls == 0 ? 0 : _stats.StructEqSuccesses * 100.0 / _stats.StructEqCalls):F1}%) (use :test result / :test clear)

        -- Memoization/Caching --
        Cache sizes:              {cacheStats}
        Cache hits/misses:        {_stats.CacheHits:#,##0} / {_stats.CacheMisses:#,##0} ({cacheHitRate})
        
        -- Performance (timings) --
        Cache lookup time:        {_stats.TimeInCacheLookup / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInCacheLookup, totalTime)})
        Substitution time:        {_stats.TimeInSubstitution / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInSubstitution, totalTime)}) (calls: {_stats.SubstitutionExprCount:#,##0})
        Evaluation time:          {_stats.TimeInEvaluation / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInEvaluation, totalTime)})
        Thunk forcing time:       {_stats.TimeInForcing / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInForcing, totalTime)})
        Total measured time:      {totalTime / 10000.0:#,##0.00} ms
        
        -- System --
        Logging:                  {_logger.LogStatus}
        Memory usage:             {memUsage:#,##0.0} KB (peak: {peakMem:#,##0.0} KB)
        Threads:                  {threads}
        Uptime:                   {upTime:dd\.hh\:mm\:ss}
        GC collections:           Gen0={gen0} Gen1={gen1} Gen2={gen2}
        Process start:            {startTime}
        """;
    }

        private static string ShowHelp()
        {
            // Build command list dynamically from metadata
            var maxSyntax = _commandMetadata.Max(c => c.Syntax.Length);
            var cmdLines = _commandMetadata
                .OrderBy(c => c.Key)
                .Select(c => $"  {c.Syntax.PadRight(maxSyntax)}  {c.Description}");
            var commandsPlain = string.Join("\n", cmdLines);

            const string multiLine = """
        -- Multi-Line Input --
            Automatic continuation when:
              * Unbalanced ( / ) or [ / ]
              * Lambda missing body after λx. or x ->
              * let binding missing 'in'
              * :macro definition missing '=>'
            Manual continuation: end a line with \\ to force join.
            Controls inside an unfinished block:
              :cancel / :abort  Discard buffer
              :show             Display current buffer
            Blank line while buffer valid attempts evaluation.
        """;

            return $"""
        ================= Lambda Calculus Interpreter Help =================

        -- Expression Syntax (abridged) --
            x                       Variable
            λx.x / \\x.x            Lambda abstraction
            x, y -> body            Arrow function (multi-parameter sugar)
            f x y                   Application (left-assoc)
            let x = A, y = B in C   Multiple let bindings (sugar for nested lambdas)
            let rec f = E in B      Recursive binding via Y
            [1,2,3] / [a .. b]      List / range / stepped range
            a |> f |> g             Pipeline (left-to-right)
            f . g . h               Composition (right associative)
            _ placeholders          Ignored lambda params auto-gensym'd
            e1; e2; e3              Top-level sequencing (commands or expressions)

        -- Commands --
        {commandsPlain}

        {multiLine}
        See README 'Formal Grammar' for precise grammar & desugarings.
        """;
        }
}
