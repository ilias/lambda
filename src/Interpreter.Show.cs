namespace LambdaCalculus;

public partial class Interpreter
{
    // Display all supported native arithmetic functions/operators
    private string ShowNativeFunctions()
        => """  
                  Supported native arithmetic functions/operators (for Church numerals) and structural helpers:

                    Binary (two arguments):
                        plus, +         : addition
                        minus, -        : subtraction (clamped to 0)
                        mult, *         : multiplication
                        div, /          : integer division (0 if divisor is 0)
                        mod, %          : modulo (0 if divisor is 0)
                        exp, pow, ^     : exponentiation
                        max             : maximum
                        min             : minimum
                        lt, <           : less than (returns true/false)
                        leq, <=         : less than or equal (returns true/false)
                        eq, ==          : equal (returns true/false)
                        geq, >=         : greater than or equal (returns true/false)
                        gt, >           : greater than (returns true/false)
                        neq, !=         : not equal (returns true/false)

                    Unary (one argument):
                        succ, ++        : successor (n+1)
                        pred, --        : predecessor (clamped to 0)
                        square          : n*n
                        double          : n*2
                        half            : n/2
                        sqrt            : integer square root
                        random          : random integer in [0, n]
                        iszero          : returns true if n==0, else false
                        even            : returns true if n is even
                        odd             : returns true if n is odd

                    Structural:
                        isStructEqual   : structural (alpha-sensitive) equality over raw ASTs (forced thunks compare forced bodies)

                    Notes:
                        - Boolean results are Church booleans (true = λf.λx.f, false = λf.λx.x)
                        - All arguments must be Church numerals (integers)
                        - isStructEqual works regardless of numeric status; counts toward native call stats
                        - These functions are only available when native arithmetic is enabled (:native on)
                """;

    private async Task<string> ShowEnv()
    {
        var envResult = await SaveFileAsync("console");
        return envResult;
    }
    
    private string ShowMacros()
    {
        var macros = _parser.ShowMacros();
        if (macros.Count == 0)
            return "No macros defined";
        
        _logger.Log("");
        foreach (var macro in macros)
            _logger.Log($"{macro}");
        return $"# Displayed {_parser._macros.Count} macros.";
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
        Structural equality:      {_stats.StructEqCalls:#,##0} calls, {_stats.StructEqSuccesses:#,##0} successes ({(_stats.StructEqCalls==0 ? 0 : _stats.StructEqSuccesses*100.0/_stats.StructEqCalls):F1}%) (use :test result / :test clear)

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

    private static string ShowHelp() =>
        """
        ================= Lambda Calculus Interpreter Help =================

        -- Expression Syntax --
          x                      Variable (e.g., myVar)
          \\x.expr or λx.expr     Lambda abstraction (e.g., \\x.x or λf.λx.f x)
          \\x y z.expr            Multi-argument lambda (sugar for \\x.\\y.\\z.expr)
          x -> expr              Arrow function (sugar for \\x.expr, e.g., x -> x + 1)
          x, y -> expr           Multi-parameter arrow function (sugar for \\x.\\y.expr)
          (expr)                 Grouping (e.g., (\\x.x) y)
          expr1 expr2            Application (e.g., succ 0)
          let x = expr1 in expr2 Local binding (e.g., let id = \\x.x in id 0) sugar for (\\x.expr2) expr1
          let x = a, y = b in B  Multiple assignments in let (e.g., let x = 1, y = 2 in x + y) sugar for (\\x.\\y.B) a b
          let f = x -> x+1 in e1 Arrow functions in let (e.g., let add = x, y -> x + y in add 3 4)
          let rec f = E in B     Recursive local binding desugar to (\\f.B) (Y (\\f.E))
          name = expr            Assignment (e.g., id = \\x.x)
          123                    Integer literal (Church numeral λf.λx.f^n(x))
          [a, b, c]              List literal (cons a (cons b (cons c nil)))
          [a .. b]               List range (syntactic sugar for [a, a+1, ..., b]) both asc and desc
          [expr1 .. expr2]       General range (desugars to (range expr1 expr2) if endpoints not both integer literals)
          [a, b .. c]            Stepped range: expands if all integers else desugars to (range2 a b c)
          Y f1                   Y combinator (e.g., Y \\f.\\x.f (f x)) Y = λf.(λx.f (x x)) (λx.f (x x))
          a + b                  Infix operations (when operators are defined) desugar to plus a b
          a . b . c              composition operator desugar to a (b c)
          a |> f |> g            Pipeline operator desugar to g (f a)
          \\_ . expr              Use '_' as a placeholder/ignored parameter in lambdas
          (x, _, _ -> x) 42 9 8  Multiple '_'s are allowed; each is treated as a unique, ignorable variable
          expr1; expr2; expr3    Multiple expressions / assignments on one line separated by ';'

        -- Minimal Grammar (see README Formal Grammar for full version) --
          Expression    ::= LetExpr | ArrowExpr | InfixExpr
          LetExpr       ::= 'let' ('rec')? LetBinding (',' LetBinding)* 'in' Expression
          LetBinding    ::= Identifier '=' Expression | Identifier ParamList '->' Expression
          ArrowExpr     ::= ParamList '->' Expression
          ParamList     ::= Param (',' Param)* | '(' Param (',' Param)* ')'
          Param         ::= Identifier | '_'
          InfixExpr     ::= Application (InfixOp Application)*
          Application   ::= Atom+
          Atom          ::= Integer | Identifier | Lambda | List | '(' Expression ')'
          Lambda        ::= (λ|\\) Param+ '.' Expression

        -- Commands (prefix with ':') --
          :clear                 Clear the current environment and caches
          :depth [n]             Set/show max recursion depth (default: 100, range: 10-10000)
          :env                   Show current environment definitions
          :exit, :quit           Exit the interpreter
          :help                  Show this help message
          :infix [op prec assoc] Define/show infix operators (e.g., :infix + 6 left)
          :lazy on|off           Toggle lazy evaluation (default: on) or (eager evaluation)
          :load <file>           Load definitions from file (e.g., :load stdlib.lambda)
          :log <file|off>        Log output to file or disable logging (e.g., :log session.log, :log off)
          :log clear             Clear the current log file (if enabled)
          :macro (pattern) => transformation  Define a macro (e.g., :macro (when $cond $body) => (if $cond $body unit))
          :macros                List all defined macros
          :memo                  Clear all memoization/caches
          :multiline             Show detailed multi-line input help and examples
          :native on|off         Enable/disable native arithmetic for Church numerals (default: on)
          :native show           Show all supported native arithmetic functions/operators
          :pretty on|off         Toggle pretty printing (default: on) - numerals and lists
          :save <file>           Save current environment to file (e.g., :save myenv.lambda)
          :stats                 Show detailed performance and environment statistics
          :step on|off           Toggle step-by-step evaluation logging
          :test clear/result     Reset or show counts of the structural equality test counters 

        -- Macro System --
          :macro (name $var1 $var2) => transformation
                                 Define a macro with pattern matching
          Examples:
            :macro (when $cond $body) => (if $cond $body unit)
            :macro (compose $f $g) => (\\x. $f ($g x))
            :macro (flip $f) => (\\x y. $f y x)
            :macro (for $var at $list do $body) => (map (\\$var.$body) $list)
            :macro (iff $p then $then else $else) => if $p $then $else

        -- Interactive Features --
          • Multi-line input: 
            - Expressions automatically continue if parentheses/brackets are unmatched
            - Lambda expressions continue until body is complete (λx. <body>)
            - Let expressions continue until 'in' clause is complete
            - Macro definitions continue until '=>' and body are complete
            - Use '\\' at end of line for explicit continuation
            - Type ':cancel' or ':abort' to discard current multi-line input
            - Type ':show' to display current multi-line input buffer
            - Press Enter on empty line to attempt completion of current input

          • Comments: Lines starting with '#' are ignored, or any text after '#' in a line is ignored
          • Command line arguments: Treated as files to load at startup
          • Infix operators: Define custom operators with precedence (1-10) and associativity (left/right)
          • Macro variables: Use $variable in patterns to capture expressions
          • Internally, each '_' is renamed to a unique variable (_placeholder1, _placeholder2, ...)

        For the complete formal grammar & desugaring rules see README section "Formal Grammar".
        """;

    private static string ShowMultiLineHelp() =>
            """
            ===== Multi-Line Input System =====

            The interpreter supports intelligent multi-line input with automatic completion detection:

            -- Automatic Continuation --
            Input automatically continues when:
            • Parentheses are unmatched: (expr1 (expr2 
            • Brackets are unmatched: [list element1
            • Lambda expressions are incomplete: λx.
            • Let expressions lack 'in': let x = 5
            • Macro definitions lack '=>': :macro (when $cond $body)

            -- Manual Continuation --
            • Use '\\' at end of line for explicit continuation
            • The interpreter shows enhanced prompts:
                lambda> first line
                ......> [2] second line (numbers are visual aids, not part of input)
            • Blank line attempts completion of current buffer

            -- Buffer Management --
            :cancel / :abort   Discard current multi-line buffer
            :show               Display current collected lines

            -- Tips --
            • Keep parentheses balanced for faster detection
            • Complex let / macro bodies can span many lines; indentation is optional but recommended
            • You can chain top-level segments with ';' only after completion, never mid buffer
            """;
}
