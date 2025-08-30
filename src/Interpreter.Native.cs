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

    private void RegisterNativeFunctions()
    {
    // Delegate to central registry (descriptor based) for clarity & discoverability
    NativeRegistry.RegisterAll(this);
    }


}
