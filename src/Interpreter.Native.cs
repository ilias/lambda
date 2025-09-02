namespace LambdaCalculus;

public partial class Interpreter
{
    // Native alpha equivalence / structural equality (works for any two expressions)
    // Pattern: (alphaEq a b) -> Church true/false based on alpha-equivalence of normalized graphs
    // We evaluate both operands fully (respecting current lazy/native settings) to compare

    internal Expr? IsRandom(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
            int min, max;
            if (args.Count == 1 && TryGetChurchInt(args[0], env, out max))
                min = 0;
            else if (args.Count == 2 && TryGetChurchInt(args[0], env, out min) && TryGetChurchInt(args[1], env, out max))
                (min, max) = (Math.Min(min, max), Math.Max(min, max));
            else
                return null;

            _usedRandom = true; // Mark that random has been used and prevent saving values 
            return MakeChurchNumeral(new Random().Next(min, max + 1));
    }

    // Beta normal form equivalence: normalize both sides (beta reduction) then alpha compare.

    // Hash equivalence: compare canonical DeBruijn-based hash representations of beta-normal forms.

    // Eta equivalence: beta-normalize both, perform eta reduction, then alphaEq.

    private void RegisterNativeFunctions()
    {
    // Delegate to central registry (descriptor based) for clarity & discoverability
    NativeRegistry.RegisterAll(this);
    }

    // Helper: determine Church boolean (assumes normal form λt.λf.t / λt.λf.f)
    private bool IsChurchTrue(Expr expr, Dictionary<string, Expr> env)
    {
        expr = expr.Type == ExprType.Thunk ? Force(expr) : expr;
        if (expr.Type != ExprType.Abs || expr.AbsBody is null) return false;
        var body = expr.AbsBody;
        body = body.Type == ExprType.Thunk ? Force(body) : body;
        if (body.Type != ExprType.Abs || body.AbsBody is null) return false;
        // true := λt.λf.t (inner body is Var bound to outer param)
        // false := λt.λf.f
        // We compare by position: inner body must be Var referencing one of the two parameters.
        // Retrieve names
        var first = expr.AbsVarName;
        var second = body.AbsVarName;
        var inner = body.AbsBody;
        if (inner.Type == ExprType.Var && inner.VarName == first) return true; // true branch
        if (inner.Type == ExprType.Var && inner.VarName == second) return false; // false branch
        return false;
    }

    // --- List Native Implementations ---------------------------------------------------------
    // Helper: attempt to deconstruct a Church-encoded list into a C# list of Expr (strict)
    private bool TryMaterializeList(Expr expr, Dictionary<string, Expr> env, out List<Expr> items)
    {
        items = new List<Expr>();
        var cur = EvaluateCEK(expr, env);
        int guard = 100_000; // safety guard
        while (guard-- > 0)
        {
            cur = cur.Type == ExprType.Thunk ? Force(cur) : cur;
            if (cur.Type == ExprType.Var)
            {
                // if variable resolves to a binding, attempt one step substitution
                if (_context.TryGetValue(cur.VarName ?? string.Empty, out var bound))
                {
                    cur = bound; continue;
                }
                return false;
            }
            if (cur.Type == ExprType.Abs || cur.Type == ExprType.App || cur.Type == ExprType.YCombinator)
                return false; // not a raw cons/nil chain
            // nil representation: expect literal 'nil' variable already handled above
            break; // fallback
        }
        // Instead of complicated pattern detection reuse a small evaluator:
        // We consider list encoded as either syntactic sugar expanded cons/nil or lambda fold form.
        // For robustness, re-evaluate as (λf.λz. ...) style by applying collectors if abstraction of arity 2.
        // Simplify: only handle explicit cons/nil sugars produced by parser.
        // Walk via pattern: cons head tail represented as application chain (cons h t)
        bool changed = true;
        cur = EvaluateCEK(expr, env);
        while (changed && guard-- > 0)
        {
            changed = false;
            cur = cur.Type == ExprType.Thunk ? Force(cur) : cur;
            // cons h t is App(App(cons,h),t)
            if (cur.Type == ExprType.App && cur.AppLeft!.Type == ExprType.App && cur.AppLeft.AppLeft!.Type == ExprType.Var && cur.AppLeft.AppLeft.VarName == "cons")
            {
                var head = cur.AppLeft.AppRight!;
                var tail = cur.AppRight!;
                items.Add(head);
                cur = tail;
                changed = true;
                continue;
            }
            if (cur.Type == ExprType.Var && cur.VarName == "nil")
                return true;
        }
        if (guard <= 0) return false;
        return cur.Type == ExprType.Var && cur.VarName == "nil";
    }

    internal Expr? LengthNative(Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        return MakeChurchNumeral(items.Count);
    }
    internal Expr? AppendNative(Expr a, Expr b, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(a, env, out var left)) return null;
        if (!TryMaterializeList(b, env, out var right)) return null;
        // rebuild cons chain left + right
        Expr result = Expr.Var("nil");
        for (int i = right.Count - 1; i >= 0; i--) result = Expr.App(Expr.App(Expr.Var("cons"), right[i]), result);
        for (int i = left.Count - 1; i >= 0; i--) result = Expr.App(Expr.App(Expr.Var("cons"), left[i]), result);
        return result;
    }
    internal Expr? ReverseNative(Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        Expr result = Expr.Var("nil");
        for (int i = 0; i < items.Count; i++) result = Expr.App(Expr.App(Expr.Var("cons"), items[i]), result);
        return result;
    }
    internal Expr? MapNative(Expr fExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        var fVal = EvaluateCEK(fExpr, env);
        // Build mapped list (preserve order) by evaluating f for each element (eager)
        var mapped = new List<Expr>(items.Count);
        foreach (var e in items)
        {
            var app = Expr.App(fVal, e);
            mapped.Add(_evaluator.Evaluate(app));
        }
        Expr result = Expr.Var("nil");
        for (int i = mapped.Count - 1; i >= 0; i--) result = Expr.App(Expr.App(Expr.Var("cons"), mapped[i]), result);
        return result;
    }
    internal Expr? FilterNative(Expr pExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        var pVal = EvaluateCEK(pExpr, env);
        var kept = new List<Expr>();
        foreach (var e in items)
        {
            var res = _evaluator.Evaluate(Expr.App(pVal, e));
            if (IsChurchTrue(res, env)) kept.Add(e);
        }
        Expr result = Expr.Var("nil");
        for (int i = kept.Count - 1; i >= 0; i--) result = Expr.App(Expr.App(Expr.Var("cons"), kept[i]), result);
        return result;
    }
    internal Expr? TakeNative(Expr nExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryGetChurchInt(nExpr, env, out var n)) return null;
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        if (n < items.Count) items = items.GetRange(0, n);
        Expr result = Expr.Var("nil");
        for (int i = items.Count - 1; i >= 0; i--) result = Expr.App(Expr.App(Expr.Var("cons"), items[i]), result);
        return result;
    }
    internal Expr? DropNative(Expr nExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryGetChurchInt(nExpr, env, out var n)) return null;
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        if (n >= items.Count) return Expr.Var("nil");
        var remain = items.GetRange(n, items.Count - n);
        Expr result = Expr.Var("nil");
        for (int i = remain.Count - 1; i >= 0; i--) result = Expr.App(Expr.App(Expr.Var("cons"), remain[i]), result);
        return result;
    }
    internal Expr? AnyNative(Expr pExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        var pVal = EvaluateCEK(pExpr, env);
        foreach (var e in items)
        {
            var res = _evaluator.Evaluate(Expr.App(pVal, e));
            if (IsChurchTrue(res, env)) return MakeChurchBoolean(true);
        }
        return MakeChurchBoolean(false);
    }
    internal Expr? AllNative(Expr pExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        var pVal = EvaluateCEK(pExpr, env);
        foreach (var e in items)
        {
            var res = _evaluator.Evaluate(Expr.App(pVal, e));
            if (!IsChurchTrue(res, env)) return MakeChurchBoolean(false);
        }
        return MakeChurchBoolean(true);
    }
    internal Expr? FindNative(Expr pExpr, Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        var pVal = EvaluateCEK(pExpr, env);
        foreach (var e in items)
        {
            var res = _evaluator.Evaluate(Expr.App(pVal, e));
            if (IsChurchTrue(res, env))
            {
                // just x = pair false x; nothing = pair true true (example) — but rely on stdlib maybe
                // Fallback: construct (just e) if constructors exist else null
                if (_context.TryGetValue("just", out var just))
                    return Expr.App(just, e);
                return Expr.App(Expr.Var("just"), e); // optimistic
            }
        }
        if (_context.TryGetValue("nothing", out var nothing)) return nothing;
        return Expr.Var("nothing");
    }
    internal Expr? SumNative(Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        long sum = 0;
        foreach (var e in items)
            if (TryGetChurchInt(e, env, out var v)) sum += v; else return null;
        if (sum > int.MaxValue) sum = int.MaxValue; // clamp
        return MakeChurchNumeral((int)sum);
    }
    internal Expr? ProductNative(Expr listExpr, Dictionary<string, Expr> env)
    {
        if (!TryMaterializeList(listExpr, env, out var items)) return null;
        long prod = 1;
        foreach (var e in items)
        {
            if (TryGetChurchInt(e, env, out var v)) prod *= v; else return null;
            if (prod == 0) break;
            if (prod > int.MaxValue) { prod = int.MaxValue; break; }
        }
        return MakeChurchNumeral((int)prod);
    }


}
