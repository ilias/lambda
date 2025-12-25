namespace LambdaCalculus;

public partial class Interpreter
{
    // Minimal De Bruijn AST used for optional binder mode
    private abstract record DbExpr;
    private sealed record DbVarBound(int Index) : DbExpr;
    private sealed record DbVarFree(string Name) : DbExpr;
    private sealed record DbAbs(DbExpr Body) : DbExpr;
    private sealed record DbApp(DbExpr Left, DbExpr Right) : DbExpr;
    private sealed record DbY() : DbExpr;
    // Delegate for native functions
    public delegate Expr? NativeFunction(string name, List<Expr> args, Dictionary<string, Expr> env);

    // Registry for native functions
    private readonly Dictionary<string, NativeFunction> _nativeFunctions = new(StringComparer.Ordinal);

    // Register a native function
    public void RegisterNativeFunction(string name, NativeFunction function)
    => _nativeFunctions[name] = function;

    // Unregister a native function
    public void UnregisterNativeFunction(string name)
        => _nativeFunctions.Remove(name);

    // CEK machine state record (evaluation concern)
    public record CEKState(Expr Control, Dictionary<string, Expr> Environment, Kontinuation Kontinuation);

    internal Expr EvaluateCEK(Expr expr, Dictionary<string, Expr>? initialEnv = null)
    {
        _perfStopwatch.Restart();
        // Disable evaluation memoization for structural equality tests (alphaEq calls)
        var disableEvalCache = IsAlphaEqInvocation(expr);
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
            // Do not use eval cache for variables to ensure they always resolve
            // against the current environment or top-level context (important
            // for macro hygiene scenarios and dynamic definitions).
            if (!disableEvalCache && control.Type != ExprType.Var && GetEvalCache(control, out var cachedResult))
            {
                ApplyContinuation(cachedResult, env, kont, stateStack, ref finalResult);
                continue;
            }


            // Try native arithmetic for every application node before further evaluation
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
                    // Resolve variable from current environment; if missing, fall back to top-level context
                    // so that free variables can reference global definitions (e.g., after macro expansion).
                    var value = env.TryGetValue(v!, out var envValue)
                        ? envValue
                        : (_context.TryGetValue(v!, out var ctxValue) ? ctxValue : control);
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
            if (!disableEvalCache && !_usedRandom)
                PutEvalCache(currentStep, expr, finalResult);
            return finalResult;
        }
        throw new InvalidOperationException("CEK evaluation completed without returning a value");
    }

    private static bool IsAlphaEqInvocation(Expr expr)
    {
        // Detect application chain whose head is variable 'alphaEq'
        var cur = expr;
        while (cur.Type == ExprType.App && cur.AppLeft != null) cur = cur.AppLeft;
        return cur.Type == ExprType.Var && cur.VarName == "alphaEq";
    }

    // Native primitives (arithmetic, comparisons, structural). Certain structural tests (alphaEq) remain active even when arithmetic disabled.
    private static readonly HashSet<string> _alwaysEnabledNatives = new(StringComparer.Ordinal) { "alphaEq", "betaEq", "hashEq", "etaEq" };
    private Expr? TryNativeArithmetic(Expr app, Dictionary<string, Expr> env)
    {
        // Decompose application spine to get operator head and argument list
        var rawArgs = new List<Expr>();
        Expr? head = app;
        while (head is { Type: ExprType.App, AppLeft: var l, AppRight: var r })
        {
            rawArgs.Insert(0, r!);
            head = l;
        }
        if (head is not { Type: ExprType.Var, VarName: var opName })
            return null;

        bool isWhitelisted = _alwaysEnabledNatives.Contains(opName!);
        if (!_useNativeArithmetic && !isWhitelisted)
            return null; // arithmetic disabled and not in whitelist

        if (!_nativeFunctions.TryGetValue(opName!, out var nativeFunc))
            return null; // not a registered native name

        var args = rawArgs;
        // Only pre-normalize arguments when arithmetic features are on (performance) or not whitelisted structural op.
        if (_useNativeArithmetic && !isWhitelisted)
        {
            for (int i = 0; i < args.Count; i++)
                args[i] = EvaluateToNormalForm(args[i], env);
        }

        Expr EvaluateToNormalForm(Expr expr, Dictionary<string, Expr> env)
        {
            if (expr.Type == ExprType.Thunk)
                expr = Force(expr);
            if (expr.Type == ExprType.App)
            {
                var reduced = TryNativeArithmetic(expr, env);
                if (reduced != null)
                    return EvaluateToNormalForm(reduced, env);
            }
            return expr;
        }

        _nativeArithmetic++;
        _stats.NativeUsage[opName!] = _stats.NativeUsage.GetValueOrDefault(opName!) + 1;
        return nativeFunc(opName!, args, env);
    }

    // Force evaluation of a thunk (lazy value)
    internal Expr Force(Expr expr)
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
                    // Beta reduction: either named-binder substitution or De Bruijn substitution
                    Expr substituted;
                    if (_useDeBruijnBinder)
                    {
                        var maybeDb = BetaReduceDeBruijn(funcToApply, argument);
                        substituted = maybeDb ?? Substitute(funcToApply.AbsBody!, funcToApply.AbsVarName!, argument);
                    }
                    else
                    {
                        substituted = Substitute(funcToApply.AbsBody!, funcToApply.AbsVarName!, argument);
                    }
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
                    // Try native arithmetic at every application node
                    var nativeResult = TryNativeArithmetic(app, env);
                    if (nativeResult != null)
                    {
                        ApplyContinuation(nativeResult, env, next!, stateStack, ref finalResult);
                    }
                    else
                    {
                        ApplyContinuation(app, env, next!, stateStack, ref finalResult);
                    }
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

    // --- De Bruijn beta-reduction (optional binder mode) ---------------------------------
    private Expr? BetaReduceDeBruijn(Expr abs, Expr arg)
    {
        // Guard: only proceed if we can convert both to DB encodings we support (Var/Abs/App/Y)
        if (abs.Type != ExprType.Abs || abs.AbsBody is null) return null;
        if (!CanConvertToDb(abs.AbsBody!) || !CanConvertToDb(arg)) return null;

        var dbBody = ToDb(abs.AbsBody!, new List<string>());
        var dbArg = ToDb(arg, new List<string>());

        // Substitute [0 -> dbArg] in body, with standard shift
        var substituted = DbShift(-1, 0, DbSubst(0, DbShift(1, 0, dbArg), dbBody));
        // Convert back to named Expr using generated binder names
        var result = FromDb(substituted, 0);
        return result;

        bool CanConvertToDb(Expr e)
        {
            var stack = new Stack<Expr>();
            stack.Push(e);
            while (stack.Count > 0)
            {
                var n = stack.Pop();
                switch (n.Type)
                {
                    case ExprType.Var:
                    case ExprType.YCombinator:
                        break;
                    case ExprType.Abs:
                        if (n.AbsBody is null) return false; stack.Push(n.AbsBody);
                        break;
                    case ExprType.App:
                        if (n.AppLeft is null || n.AppRight is null) return false; stack.Push(n.AppRight); stack.Push(n.AppLeft);
                        break;
                    default:
                        return false; // Thunk/Quote/etc not supported in DB path
                }
            }
            return true;
        }

        DbExpr ToDb(Expr e, List<string> ctx)
        {
            return e.Type switch
            {
                ExprType.Var =>
                    (e.VarName is null)
                        ? new DbVarFree("_")
                        : (ctx.LastIndexOf(e.VarName) is var idx && idx >= 0
                            ? new DbVarBound(ctx.Count - 1 - idx)
                            : new DbVarFree(e.VarName)),
                ExprType.Abs =>
                    new DbAbs(ToDb(e.AbsBody!, new List<string>(ctx) { e.AbsVarName ?? "_" })),
                ExprType.App => new DbApp(ToDb(e.AppLeft!, ctx), ToDb(e.AppRight!, ctx)),
                ExprType.YCombinator => new DbY(),
                _ => throw new NotSupportedException("Unsupported node in DB conversion")
            };
        }

        DbExpr DbShift(int d, int cutoff, DbExpr expr)
        {
            return expr switch
            {
                DbVarBound vb => new DbVarBound(vb.Index >= cutoff ? vb.Index + d : vb.Index),
                DbVarFree vf => vf,
                DbAbs a => new DbAbs(DbShift(d, cutoff + 1, a.Body)),
                DbApp app => new DbApp(DbShift(d, cutoff, app.Left), DbShift(d, cutoff, app.Right)),
                DbY y => y,
                _ => expr
            };
        }

        DbExpr DbSubst(int j, DbExpr s, DbExpr expr)
        {
            return expr switch
            {
                DbVarBound vb => vb.Index == j ? s : vb,
                DbVarFree vf => vf,
                DbAbs a => new DbAbs(DbSubst(j + 1, DbShift(1, 0, s), a.Body)),
                DbApp app => new DbApp(DbSubst(j, s, app.Left), DbSubst(j, s, app.Right)),
                DbY y => y,
                _ => expr
            };
        }

        Expr FromDb(DbExpr e, int depth)
        {
            switch (e)
            {
                case DbVarBound vb:
                    // Bound var: refer to the binder name at distance vb.Index
                    var name = $"x{depth - 1 - vb.Index}";
                    return Expr.Var(name);
                case DbVarFree vf:
                    return Expr.Var(vf.Name);
                case DbAbs a:
                    var binder = $"x{depth}";
                    return Expr.Abs(binder, FromDb(a.Body, depth + 1));
                case DbApp app:
                    return Expr.App(FromDb(app.Left, depth), FromDb(app.Right, depth));
                case DbY:
                    return Expr.YCombinator();
                default:
                    throw new NotSupportedException("Unknown DB node");
            }
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


