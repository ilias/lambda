namespace LambdaCalculus;

public partial class Interpreter
{
    // Native alpha equivalence / structural equality (works for any two expressions)
    // Pattern: (alphaEq a b) -> Church true/false based on alpha-equivalence of normalized graphs
    // We evaluate both operands fully (respecting current lazy/native settings) to compare
    internal Expr? AlphaEqNative(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null; // Need exactly two arguments

        var leftVal = EvaluateCEK(args[0], env);
        var rightVal = EvaluateCEK(args[1], env);

        // Force thunks (lightweight) before normalization to avoid comparing wrapper thunks
        if (leftVal.Type == ExprType.Thunk) leftVal = Force(leftVal);
        if (rightVal.Type == ExprType.Thunk) rightVal = Force(rightVal);

        // Normalize both sides (beta-reduce) for a more canonical structural shape
        var normLeft = NormalizeExpression(leftVal);
        var normRight = NormalizeExpression(rightVal);

        // Alpha-equivalence on normalized forms
        var equal = Expr.AlphaEquivalent(normLeft, normRight);

        // If still different, optionally fall back to raw structural for diagnostic nuance
        if (!equal && Expr.AlphaEquivalent(leftVal, rightVal))
            equal = true; // normalization introduced incidental difference

        _logger.Log($"Test: alpha left:  {FormatWithNumerals(normLeft)}");
        _logger.Log($"Test: alpha right: {FormatWithNumerals(normRight)}");

        _nativeArithmetic++;
        _stats.StructEqCalls++;
        if (equal) _stats.StructEqSuccesses++;
        var total = equal ? _stats.StructEqSuccesses : _stats.StructEqCalls - _stats.StructEqSuccesses;
        var totalPercent = (double)total / _stats.StructEqCalls * 100;
        _logger.Log($"Test: alpha {(equal ? "passed" : "failed")} - {total}/{_stats.StructEqCalls} ({totalPercent:F2}%)");
        return MakeChurchBoolean(equal);
    }

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
    internal Expr? BetaEqNative(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var left = EvaluateCEK(args[0], env);
        var right = EvaluateCEK(args[1], env);
        if (left.Type == ExprType.Thunk) left = Force(left);
        if (right.Type == ExprType.Thunk) right = Force(right);
        var nL = NormalizeExpression(left);
        var nR = NormalizeExpression(right);
        var eq = Expr.AlphaEquivalent(nL, nR);
        return MakeChurchBoolean(eq);
    }

    // Hash equivalence: compare canonical DeBruijn-based hash representations of beta-normal forms.
    internal Expr? HashEqNative(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var left = EvaluateCEK(args[0], env);
        var right = EvaluateCEK(args[1], env);
        if (left.Type == ExprType.Thunk) left = Force(left);
        if (right.Type == ExprType.Thunk) right = Force(right);
        var nL = NormalizeExpression(left);
        var nR = NormalizeExpression(right);
        var hL = CanonicalHash(nL);
        var hR = CanonicalHash(nR);
        return MakeChurchBoolean(hL == hR);

        // Produce canonical string then hash; uses De Bruijn indexing for bound vars, preserves free names.
        string CanonicalHash(Expr expr)
        {
            var sb = new System.Text.StringBuilder();
            var stack = new Stack<(Expr node, List<string> ctx)>();
            stack.Push((expr, new List<string>()));
            while (stack.Count > 0)
            {
                var (node, ctx) = stack.Pop();
                switch (node.Type)
                {
                    case ExprType.Var:
                        if (node.VarName != null)
                        {
                            int idx = ctx.LastIndexOf(node.VarName);
                            if (idx >= 0)
                                sb.Append('b').Append(ctx.Count - 1 - idx).Append(';'); // bound index
                            else
                                sb.Append('f').Append(node.VarName).Append(';'); // free name
                        }
                        else sb.Append("v;");
                        break;
                    case ExprType.Abs:
                        sb.Append('λ');
                        var newCtx = new List<string>(ctx) { node.AbsVarName ?? "_" };
                        stack.Push((node.AbsBody!, newCtx));
                        break;
                    case ExprType.App:
                        sb.Append('(');
                        // Push closing marker then right then left
                        sb.Append('A');
                        sb.Append(')'); // structural marker (we'll rely on order of pushes for traversal sequence)
                        stack.Push((node.AppRight!, new List<string>(ctx)));
                        stack.Push((node.AppLeft!, new List<string>(ctx)));
                        break;
                    case ExprType.Thunk:
                        var inner = node.ThunkValue!.IsForced && node.ThunkValue.ForcedValue is not null
                            ? node.ThunkValue.ForcedValue
                            : node.ThunkValue.Expression;
                        stack.Push((inner!, new List<string>(ctx)));
                        break;
                    case ExprType.YCombinator:
                        sb.Append("Y;");
                        break;
                }
            }
            // Simple FNV-1a over the canonical string for compactness
            unchecked
            {
                const uint offset = 2166136261;
                const uint prime = 16777619;
                uint hash = offset;
                foreach (var ch in sb.ToString()) { hash ^= ch; hash *= prime; }
                return hash.ToString("X8");
            }
        }
    }

    // Eta equivalence: beta-normalize both, perform eta reduction, then alphaEq.
    internal Expr? EtaEqNative(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var l = EvaluateCEK(args[0], env);
        var r = EvaluateCEK(args[1], env);
        if (l.Type == ExprType.Thunk) l = Force(l);
        if (r.Type == ExprType.Thunk) r = Force(r);
        l = NormalizeExpression(l);
        r = NormalizeExpression(r);
        l = EtaReduce(l, 64); // depth guard
        r = EtaReduce(r, 64);
        return MakeChurchBoolean(Expr.AlphaEquivalent(l, r));

        Expr EtaReduce(Expr expr, int budget)
        {
            if (budget <= 0) return expr;
            return expr.Type switch
            {
                ExprType.Abs when expr.AbsBody is { Type: ExprType.App, AppLeft: var f, AppRight: { Type: ExprType.Var, VarName: var v } arg }
                                   && expr.AbsVarName != null && v == expr.AbsVarName
                                   && !FreeVars(f!).Contains(expr.AbsVarName!)
                                   && ( // ensure bound var only appears once at tail position
                                       CountVarOccurrences(f!, expr.AbsVarName!) == 0)
                    => EtaReduce(f!, budget - 1),
                ExprType.Abs => Expr.Abs(expr.AbsVarName!, EtaReduce(expr.AbsBody!, budget - 1)),
                ExprType.App => Expr.App(EtaReduce(expr.AppLeft!, budget - 1), EtaReduce(expr.AppRight!, budget - 1)),
                ExprType.Thunk => expr.ThunkValue!.IsForced && expr.ThunkValue.ForcedValue is not null
                                    ? EtaReduce(expr.ThunkValue.ForcedValue!, budget - 1)
                                    : EtaReduce(expr.ThunkValue.Expression, budget - 1),
                _ => expr
            };
        }

        int CountVarOccurrences(Expr e, string varName)
        {
            int count = 0;
            var stack = new Stack<Expr>();
            stack.Push(e);
            while (stack.Count > 0)
            {
                var cur = stack.Pop();
                switch (cur.Type)
                {
                    case ExprType.Var when cur.VarName == varName: count++; break;
                    case ExprType.Abs when cur.AbsBody is not null && cur.AbsVarName != varName:
                        stack.Push(cur.AbsBody); break; // if abs binds varName, skip body for counting occurrences outside correct position
                    case ExprType.App:
                        stack.Push(cur.AppLeft!); stack.Push(cur.AppRight!); break;
                    case ExprType.Thunk:
                        if (cur.ThunkValue != null)
                            stack.Push(cur.ThunkValue.IsForced && cur.ThunkValue.ForcedValue is not null ? cur.ThunkValue.ForcedValue! : cur.ThunkValue.Expression);
                        break;
                }
            }
            return count;
        }
    }

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
