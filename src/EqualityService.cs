using System;
using System.Collections.Generic;

namespace LambdaCalculus;

/// <summary>
/// Encapsulates equality / equivalence native operations (alpha, beta, hash, eta) so the core
/// interpreter class remains slimmer. All methods are pure relative to the supplied interpreter
/// instance (used for evaluation, logging + helpers).
/// </summary>
internal sealed class EqualityService
{
    private readonly Interpreter _interp;
    private readonly Logger _logger;

    internal EqualityService(Interpreter interp)
    {
        _interp = interp;
        _logger = interp.LoggerInstance; // expose an internal accessor rather than using reflection
    }

    internal Expr? AlphaEq(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var leftVal = _interp.EvaluateCEK(args[0], env);
        var rightVal = _interp.EvaluateCEK(args[1], env);
        if (leftVal.Type == ExprType.Thunk) leftVal = _interp.Force(leftVal);
        if (rightVal.Type == ExprType.Thunk) rightVal = _interp.Force(rightVal);
        var normLeft = _interp.NormalizeExpression(leftVal);
        var normRight = _interp.NormalizeExpression(rightVal);
    var equal = Expr.AlphaEquivalent(normLeft, normRight) || (!Expr.AlphaEquivalent(normLeft, normRight) && Expr.AlphaEquivalent(leftVal, rightVal));
    return _interp.MakeChurchBoolean(LogAndUpdateStructEq("alpha", normLeft, normRight, equal));
    }

    internal Expr? BetaEq(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var left = _interp.EvaluateCEK(args[0], env);
        var right = _interp.EvaluateCEK(args[1], env);
        if (left.Type == ExprType.Thunk) left = _interp.Force(left);
        if (right.Type == ExprType.Thunk) right = _interp.Force(right);
        var nL = _interp.NormalizeExpression(left);
        var nR = _interp.NormalizeExpression(right);
    var equal = Expr.AlphaEquivalent(nL, nR);
    return _interp.MakeChurchBoolean(LogAndUpdateStructEq("beta", nL, nR, equal));
    }

    internal Expr? HashEq(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var left = _interp.EvaluateCEK(args[0], env);
        var right = _interp.EvaluateCEK(args[1], env);
        if (left.Type == ExprType.Thunk) left = _interp.Force(left);
        if (right.Type == ExprType.Thunk) right = _interp.Force(right);
        var nL = _interp.NormalizeExpression(left);
        var nR = _interp.NormalizeExpression(right);
    // Logging will be handled by helper after computing equality outcome
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
                            if (idx >= 0) sb.Append('b').Append(ctx.Count - 1 - idx).Append(';');
                            else sb.Append('f').Append(node.VarName).Append(';');
                        }
                        else sb.Append("v;");
                        break;
                    case ExprType.Abs:
                        sb.Append('λ');
                        var newCtx = new List<string>(ctx) { node.AbsVarName ?? "_" };
                        stack.Push((node.AbsBody!, newCtx));
                        break;
                    case ExprType.App:
                        sb.Append('(').Append('A').Append(')');
                        stack.Push((node.AppRight!, new List<string>(ctx)));
                        stack.Push((node.AppLeft!, new List<string>(ctx)));
                        break;
                    case ExprType.Thunk:
                        var inner = node.ThunkValue!.IsForced && node.ThunkValue.ForcedValue is not null ? node.ThunkValue.ForcedValue : node.ThunkValue.Expression;
                        stack.Push((inner!, new List<string>(ctx)));
                        break;
                    case ExprType.YCombinator:
                        sb.Append("Y;");
                        break;
                }
            }
            unchecked
            {
                const uint offset = 2166136261;
                const uint prime = 16777619;
                uint hash = offset;
                foreach (var ch in sb.ToString()) { hash ^= ch; hash *= prime; }
                return hash.ToString("X8");
            }
        }
        var hL = CanonicalHash(nL);
        var hR = CanonicalHash(nR);
    var hashEq = hL == hR;
    return _interp.MakeChurchBoolean(LogAndUpdateStructEq("hash", nL, nR, hashEq));
    }

    internal Expr? EtaEq(string op, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count != 2) return null;
        var l = _interp.EvaluateCEK(args[0], env);
        var r = _interp.EvaluateCEK(args[1], env);
        if (l.Type == ExprType.Thunk) l = _interp.Force(l);
        if (r.Type == ExprType.Thunk) r = _interp.Force(r);
        l = _interp.NormalizeExpression(l);
        r = _interp.NormalizeExpression(r);
        l = EtaReduce(l, 64);
        r = EtaReduce(r, 64);
    var etaEq = Expr.AlphaEquivalent(l, r);
    return _interp.MakeChurchBoolean(LogAndUpdateStructEq("eta", l, r, etaEq));

        Expr EtaReduce(Expr expr, int budget)
        {
            if (budget <= 0) return expr;
            return expr.Type switch
            {
                ExprType.Abs when expr.AbsBody is { Type: ExprType.App, AppLeft: var f, AppRight: { Type: ExprType.Var, VarName: var v } arg }
                                   && expr.AbsVarName != null && v == expr.AbsVarName
                                   && !_interp.FreeVars(f!).Contains(expr.AbsVarName!)
                                   && CountVarOccurrences(f!, expr.AbsVarName!) == 0
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
                        stack.Push(cur.AbsBody); break;
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

    private bool LogAndUpdateStructEq(string kind, Expr leftNorm, Expr rightNorm, bool equal)
    {
        _logger.Log($"Test: {kind} left:  {_interp.FormatWithNumerals(leftNorm)}");
        _logger.Log($"Test: {kind} right: {_interp.FormatWithNumerals(rightNorm)}");
        _interp._nativeArithmetic++;
        _interp._stats.StructEqCalls++;
        if (kind != "alpha" || equal) _interp._stats.StructEqSuccesses++;
        var total = equal ? _interp._stats.StructEqSuccesses : _interp._stats.StructEqCalls - _interp._stats.StructEqSuccesses;
        var totalPercent = (double)total / _interp._stats.StructEqCalls * 100;
        _logger.Log($"Test: {kind} {(equal ? "passed" : "failed")} - {total}/{_interp._stats.StructEqCalls} ({totalPercent:F2}%)");
        return equal;
    }
}
