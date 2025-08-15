namespace LambdaCalculus;

public partial class Interpreter
{
    // Native structural equality (works for any two expressions)
    // Pattern: (isStructEqual a b) -> Church true/false based on structural graph equality
    // We evaluate both operands fully (respecting current lazy/native settings) to compare
    internal Expr? IsStructEqual(List<Expr> args, Dictionary<string, Expr> env)
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

        _logger.Log($"Test: left:  {FormatWithNumerals(normLeft)}");
        _logger.Log($"Test: right: {FormatWithNumerals(normRight)}");
        _logger.Log($"Test: {(equal ? "passed" : "failed")}");

        _nativeArithmetic++;
        _stats.StructEqCalls++;
        if (equal) _stats.StructEqSuccesses++;
        return MakeChurchBoolean(equal);
    }

    // Intercept known arithmetic primitives
    internal int? ArithmeticPrimitives(string? opName, int a, int b, List<Expr> args, bool isArg2Number)
        => (opName, args.Count, isArg2Number) switch
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
            ("square", 1, _) => a * a,
            ("half", 1, _) => a / 2,
            ("sqrt", 1, _) => (int)Math.Sqrt(a),
            ("random", 1, _) => (_usedRandom = true, new Random().Next(0, a + 1)).Item2, // Random number in range [0, a]

            _ => null
        };

    internal bool? ComparisonPrimitives(string? opName, int a, int b, List<Expr> args, bool isArg2Number)
        => (opName, args.Count, isArg2Number) switch
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

    private void RegisterNativeFunctions()
    {
        RegisterNativeFunction("inc", (args, env) =>
        {
            if (args.Count == 1 && TryGetChurchInt(args[0], env, out var n))
                return MakeChurchNumeral(n + 1);
            return null;
        });

        RegisterNativeFunction("dec", (args, env) =>
        {
            if (args.Count == 1 && TryGetChurchInt(args[0], env, out var n))
                return MakeChurchNumeral(n - 1);
            return null;
        });

        RegisterNativeFunction("isStructEqual", IsStructEqual);
    }
}