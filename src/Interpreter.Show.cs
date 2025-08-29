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
        static string Percent(long value, long total) => total <= 0 ? "0.0%" : $"{value * 100.0 / total:F1}%";
        static string Bar(double fraction, int width = 24)
        {
            if (fraction < 0) fraction = 0; if (fraction > 1) fraction = 1;
            var filled = (int)Math.Round(fraction * width);
            return new string('█', filled) + new string('·', width - filled);
        }

        long totalTimeTicks = _stats.TimeInCacheLookup + _stats.TimeInSubstitution + _stats.TimeInEvaluation + _stats.TimeInForcing;
        double ToMs(long ticks) => ticks / 10_000.0; // 1 tick = 100ns

        var proc = System.Diagnostics.Process.GetCurrentProcess();
        var now = DateTime.Now;
        var up = now - proc.StartTime;
        double memKB = GC.GetTotalMemory(false) / 1024.0;
        double peakKB = proc.PeakWorkingSet64 / 1024.0;
        double memMB = memKB / 1024.0;
        double peakMB = peakKB / 1024.0;
        var evalMode = _lazyEvaluation ? "Lazy" : "Eager";
        long cacheTotal = _stats.CacheHits + _stats.CacheMisses;
        double cacheHitFrac = cacheTotal == 0 ? 0 : (double)_stats.CacheHits / cacheTotal;
        double structEqFrac = _stats.StructEqCalls == 0 ? 0 : (double)_stats.StructEqSuccesses / _stats.StructEqCalls;

        // Collect macro/native usage (top 10 each)
        var macroLines = _stats.MacroUsage
            .OrderByDescending(k => k.Value)
            .Take(10)
            .Select(k => $"  {k.Key,-24} {k.Value,8:#,##0}");
        var nativeLines = _stats.NativeUsage
            .OrderByDescending(k => k.Value)
            .Take(10)
            .Select(k => $"  {k.Key,-24} {k.Value,8:#,##0}");

        string Section(string title) => $"\n── {title} " + new string('─', Math.Max(0, 68 - title.Length));
        string Line(string label, object value) => $"  {label.PadRight(28)} {value}";
        string TimeLine(string label, long ticks)
        {
            var pct = Percent(ticks, totalTimeTicks);
            var frac = totalTimeTicks == 0 ? 0 : (double)ticks / totalTimeTicks;
            return $"  {label.PadRight(20)} {ToMs(ticks),10:#,##0.00} ms  {pct,7}  {Bar(frac)}";
        }

        var cacheSizes = new[]
        {
            ($"Substitution", _substitutionCache.Count),
            ($"Evaluation", _evaluationCache.Count),
            ($"FreeVar", _freeVarCache.Count),
            ($"VarPool", _expressionPool.Count),
            ($"ContainsVar", _containsVarCache.Count),
            ($"Normalization", _normalizationCache.Count)
        };
        var cacheSizeLines = cacheSizes.Select(c => $"  {c.Item1,-14} {c.Item2,8:#,##0}");

        var sb = new System.Text.StringBuilder();
        sb.AppendLine("╔══════════════════════════════════════════════════════════════════════╗");
        sb.AppendLine("║  Lambda Interpreter Statistics                                       ║");
        sb.AppendLine("╚══════════════════════════════════════════════════════════════════════╝");

        // SUMMARY
        sb.AppendLine(Section("Summary"));
        sb.AppendLine(Line("Mode", evalMode));
        sb.AppendLine(Line("Definitions", _context.Count.ToString("#,##0")));
        sb.AppendLine(Line("Infix operators", _parser._infixOperators.Count.ToString("#,##0")));
        sb.AppendLine(Line("Native arithmetic", _useNativeArithmetic ? "ENABLED" : "DISABLED"));
        sb.AppendLine(Line("Pretty printing", _prettyPrint ? "ENABLED" : "DISABLED"));
        sb.AppendLine(Line("Unique expressions", _expressionPool.Count.ToString("#,##0")));
        sb.AppendLine(Line("Fresh var counter", _stats.VarCounter.ToString("#,##0")));
        sb.AppendLine(Line("Uptime", up.ToString(@"dd\.hh\:mm\:ss")));
        sb.AppendLine(Line("Memory (working)", $"{memMB:#,##0.0} MB (peak {peakMB:#,##0.0} MB)"));

        // EVALUATION
        sb.AppendLine(Section("Evaluation"));
        sb.AppendLine(Line("Recursion depth limit", _stats.MaxRecursionDepth.ToString("#,##0")));
        sb.AppendLine(Line("Total iterations", _stats.TotalIterations.ToString("#,##0")));
        sb.AppendLine(Line("Normalizations", _stats.NormalizeCEKCount.ToString("#,##0")));
        sb.AppendLine(Line("Thunks forced", _stats.ThunkForceCount.ToString("#,##0")));
        sb.AppendLine(Line("Hash code calls", Expr.HashCodeCount.ToString("#,##0")));
        sb.AppendLine(Line("Native arithmetic calls", _nativeArithmetic.ToString("#,##0")));
        sb.AppendLine(Line("Alpha equivalence tests", $"{_stats.StructEqCalls:#,##0} (success {Percent(_stats.StructEqSuccesses, _stats.StructEqCalls)} )"));
        sb.AppendLine(Line("Alpha equivalence success", Bar(structEqFrac)));
        sb.AppendLine(Line("Step mode", _showStep ? "ENABLED" : "DISABLED"));

        // PERFORMANCE
        sb.AppendLine(Section("Timing (inclusive)"));
        sb.AppendLine("  Component              Time (ms)    Share    Distribution");
        sb.AppendLine(TimeLine("Cache lookup", _stats.TimeInCacheLookup));
        sb.AppendLine(TimeLine("Substitution", _stats.TimeInSubstitution));
        sb.AppendLine(TimeLine("Evaluation", _stats.TimeInEvaluation));
        sb.AppendLine(TimeLine("Thunk forcing", _stats.TimeInForcing));
        sb.AppendLine(TimeLine("TOTAL", totalTimeTicks));

        // CACHES
        sb.AppendLine(Section("Caches"));
        sb.AppendLine(Line("Hit / Miss", $"{_stats.CacheHits:#,##0} / {_stats.CacheMisses:#,##0} ({Percent(_stats.CacheHits, cacheTotal)})"));
        sb.AppendLine(Line("Cache hit ratio", Bar(cacheHitFrac)));
        foreach (var l in cacheSizeLines) sb.AppendLine(l);

        // USAGE (combined)
        if (_stats.MacroUsage.Count > 0 || _stats.NativeUsage.Count > 0)
        {
            sb.AppendLine(Section("Usage (top 10 each)"));
            sb.AppendLine("  Macro Name                Uses    │  Native Name               Uses");
            sb.AppendLine("  ------------------------- --------┼------------------------- --------");
            // Pair rows side-by-side
            var macroArr = macroLines.ToList();
            var nativeArr = nativeLines.ToList();
            var rows = Math.Max(macroArr.Count, nativeArr.Count);
            for (int i = 0; i < rows; i++)
            {
                var left = i < macroArr.Count ? macroArr[i] : string.Empty;
                var right = i < nativeArr.Count ? nativeArr[i] : string.Empty;
                // Strip leading double spaces used for indentation in original lines
                if (left.StartsWith("  ")) left = left[2..];
                if (right.StartsWith("  ")) right = right[2..];
                left = left.PadRight(34); // ensure alignment (name+spaces to before separator)
                sb.AppendLine($"  {left}│  {right}");
            }
        }

        // SYSTEM
        sb.AppendLine(Section("System"));
        sb.AppendLine(Line("Logging", _logger.LogStatus));
        sb.AppendLine(Line("Threads", proc.Threads.Count.ToString("#,##0")));
        sb.AppendLine(Line("GC Collections", $"Gen0={GC.CollectionCount(0)} Gen1={GC.CollectionCount(1)} Gen2={GC.CollectionCount(2)}"));
        sb.AppendLine(Line("Process start", proc.StartTime.ToString()));

        return sb.ToString();
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
            x                         Variable
            λx.x / \\x.x              Lambda abstraction
            x, y -> body              Arrow function (multi-parameter sugar)
            def f x y = body          Function definition sugar (desugars to f = x,y -> body)
            f x y                     Application (left-assoc)
            f $ x $ y                 Application via low‑precedence right-assoc operator ($)
            let x = A, y = B in C     Multiple let bindings (sugar for nested λ abstractions)
            let rec f = E in B        Recursive binding (desugars to let f = Y (λf.E) in B)
            [1,2,3] / [a .. b]        List literal / inclusive numeric range
            [a .. b .. s]             Stepped range (step s)
            a |> f |> g               Pipeline (desugars to g (f a))
            f . g . h                 Composition (desugars to f (g (h x)) when applied; right-assoc)
            _ placeholders            Ignored lambda params auto-gensym'd (λ_._ -> λα0.α0)
            e1; e2; e3                Sequential evaluation (each printed)

        -- Built‑In Infix Operators --
            |>   (pipeline, left, precedence 2)
            .    (composition, right, precedence 3)
            $    (application, right, precedence 1 – lowest)
            (User operators definable via :infix name prec assoc)

        -- Commands --
        {commandsPlain}

        -- Command Effects / Desugarings --
            :clear                ≈ reset (macros|defs|ops|cache|stats) scope; :clear macros clears only macros, etc.
            :depth n              Set maximum recursion depth guard to n
            :env part             Show environment subset (defs, macros, infix, native, all)
            :exit / :quit         Terminate process
            :infix op p a         Register infix op (precedence p, associativity a)
            :lazy on|off          Toggle evaluation strategy (on = lazy, off = eager)
            :load file            Read file; each line evaluated (macros & :infix allowed)
            :log file|off|clear   Append output to file / disable / truncate
            :macro (pat) => body  Add macro rewrite rule (applied before parse segment)
            :native on|off|show   Toggle/show native arithmetic & list primitives
            :pretty on|off        Toggle pretty printing of numerals/lists/booleans
            :save file            Persist current defs/macros/infix to file
            :stats                Display performance metrics & cache sizes
            :step on|off          Enable CEK machine step trace output
            :test clear|result    Reset or show structural equality test counters
            :help                 Show this summary

        -- Key Desugarings (Summary) --
            def f x y = body    =>  f = x,y -> body
            x, y -> body        =>  x -> y -> body
            let x = A, y = B in C  =>  let x = A in let y = B in C
            let rec f = E in B  =>  let f = Y (λf. E) in B
            a |> f |> g         =>  g (f a)
            f . g . h           =>  f . (g . h)  (application: (f . g) x => f (g x))
            f $ x $ y           =>  f x y  (right-assoc, lowest precedence)
            λ_._ or λ_,x._      =>  λα0.α1...  (anonymous fresh names ignored if unused)

        {multiLine}
        See README 'Formal Grammar' for precise grammar & full desugarings.
        """;
        }
}
