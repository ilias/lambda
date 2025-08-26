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

        _logger.Log($"Test: left:  {FormatWithNumerals(normLeft)}");
        _logger.Log($"Test: right: {FormatWithNumerals(normRight)}");

        _nativeArithmetic++;
        _stats.StructEqCalls++;
        if (equal) _stats.StructEqSuccesses++;
        var total = equal ? _stats.StructEqSuccesses : _stats.StructEqCalls - _stats.StructEqSuccesses;
        var totalPercent = (double)total / _stats.StructEqCalls * 100;
        _logger.Log($"Test: {(equal ? "passed" : "failed")} - {total}/{_stats.StructEqCalls} ({totalPercent:F2}%)");
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

    internal Expr? IsArithmeticPrimitive(string opName, List<Expr> args, Dictionary<string, Expr> env)
    {
        if (args.Count < 1 || args.Count > 2) return null; // Invalid number of arguments

        // Try to get the first argument as a Church numeral
        if (!TryGetChurchInt(args[0], env, out int a)) return null;

        // If there's a second argument, try to get it as well
        int b = 0;
        bool isArg2Number = false;
        if (args.Count == 2 && TryGetChurchInt(args[1], env, out b))
            isArg2Number = true;

        // Check for known arithmetic primitives
        var result = ArithmeticPrimitives(opName, a, b, args, isArg2Number);
        if (result.HasValue) return MakeChurchNumeral(result.Value);

        // Check for known comparison primitives
        var comparisonResult = ComparisonPrimitives(opName, a, b, args, isArg2Number);
        if (comparisonResult.HasValue) return MakeChurchBoolean(comparisonResult.Value);

        return null; // Not an arithmetic or comparison primitive
    }
    // Intercept known arithmetic primitives
    internal static int? ArithmeticPrimitives(string? opName, int a, int b, List<Expr> args, bool isArg2Number)
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

            ("succ" or "++" or "inc", 1, _) => a + 1,
            ("pred" or "--" or "decr", 1, _) => Math.Max(0, a - 1),
            ("square", 1, _) => a * a,
            ("half", 1, _) => a / 2,
            ("sqrt", 1, _) => (int)Math.Sqrt(a),

            _ => null
        };

    internal static bool? ComparisonPrimitives(string? opName, int a, int b, List<Expr> args, bool isArg2Number)
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
        RegisterNativeFunction("random", IsRandom);
        RegisterNativeFunction("alphaEq", AlphaEqNative);

        List<string> nativeArithmeticOps =
        [
            "plus", "+",
            "minus", "-",
            "mult", "*",
            "div", "/",
            "mod", "%",
            "exp", "pow", "^",
            "max", "min",
            "succ", "++", "inc",
            "pred", "--", "decr",
            "square", "half", "sqrt",

            "lt", "<",
            "leq", "<=",
            "eq", "==",
            "geq", ">=",
            "gt", ">",
            "neq", "!=",
            "iszero", "even", "odd"
        ];
        // Arithmetic primitives
        foreach (var op in nativeArithmeticOps)
            RegisterNativeFunction(op, IsArithmeticPrimitive);

    }


}
