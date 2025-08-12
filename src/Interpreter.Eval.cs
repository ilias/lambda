namespace LambdaCalculus;

public partial class Interpreter
{
    // CEK machine state record (evaluation concern)
    public record CEKState(Expr Control, Dictionary<string, Expr> Environment, Kontinuation Kontinuation);

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
            ("square" or "dbl" or "double", 1, _) => a * a,
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
            ("lt" or "<", 2, true) => a < b,
            ("leq" or "<=", 2, true) => a <= b,
            ("eq" or "==", 2, true) => a == b,
            ("geq" or ">=", 2, true) => a >= b,
            ("gt" or ">", 2, true) => a > b,
            ("neq" or "!=", 2, true) => a != b,

            ("iszero", 1, _) => a == 0,
            ("even", 1, _) => a % 2 == 0,
            ("odd", 1, _) => a % 2 != 0,

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

    private void PutEvalCache(int step, Expr expr, Expr result) => _evaluationCache.TryAdd(expr, result);
    private bool GetEvalCache(Expr expr, out Expr result) => GetFromCache(_evaluationCache, expr, out result);

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
