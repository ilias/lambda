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


}
