namespace LambdaCalculus;

// Church numeral / boolean / conditional related helpers extracted from Interpreter
public partial class Interpreter
{
    // Build a Church numeral expression for a given int λf.λx.f^n(x)
    private Expr MakeChurchNumeral(int n)
    {
        var f = "f";
        var x = "x";
        Expr body = Expr.Var(x);
        for (int i = 0; i < n; i++)
            body = Expr.App(Expr.Var(f), body);
        return Expr.Abs(f, Expr.Abs(x, body));
    }

    // Try to extract a Church numeral as int, resolving variables if needed
    private bool TryGetChurchInt(Expr expr, Dictionary<string, Expr> env, out int value)
    {
        // Resolve variables
        while (expr.Type == ExprType.Var && env.TryGetValue(expr.VarName!, out var v))
            expr = v;
        var n = ExtractChurchNumeralValue(expr);
        if (n != null)
        {
            value = n.Value;
            return true;
        }
        value = 0;
        return false;
    }

    // Returns the integer value of a Church numeral (λf.λx.f^n(x)), or null if not valid.
    public static int? ExtractChurchNumeralValue(Expr expr)
    {
        // Force evaluation if it's a thunk
        if (expr.Type == ExprType.Thunk)
            expr = expr.ThunkValue?.IsForced == true ? expr.ThunkValue.ForcedValue! : expr;

        if (expr is not { Type: ExprType.Abs, AbsVarName: var f, AbsBody: { Type: ExprType.Abs, AbsVarName: var x, AbsBody: var body } })
            return null;
        if (body is { Type: ExprType.Var, VarName: var v } && v == x)
            return 0;
        int n = 0;
        while (body is { Type: ExprType.App, AppLeft: { Type: ExprType.Var, VarName: var fn }, AppRight: var next } && fn == f)
        {
            n++;
            body = next;
        }
        return body is { Type: ExprType.Var, VarName: var v2 } && v2 == x ? n : null;
    }

    // Detect if this is a Church conditional (if p a b) 
    private bool IsChurchConditional(Expr expr, Dictionary<string, Expr> env)
    {
        // Check if this is a fully applied Church conditional: App(App(App(if, condition), then_branch), else_branch)
        if (expr is not { Type: ExprType.App, AppLeft: { Type: ExprType.App, AppLeft: { Type: ExprType.App, AppLeft: var ifExpr } } })
            return false;

        // Check if the leftmost expression resolves to the 'if' function
        if (ifExpr == null) return false;
        var resolvedIf = ResolveVariable(ifExpr, env);
        return IsIfFunction(resolvedIf);
    }

    // Check if an expression is the Church 'if' function: λp.λa.λb.p a b
    private static bool IsIfFunction(Expr expr) =>
        expr is
        {
            Type: ExprType.Abs,
            AbsVarName: var p,
            AbsBody:
            {
                Type: ExprType.Abs,
                AbsVarName: var a,
                AbsBody:
                {
                    Type: ExprType.Abs,
                    AbsVarName: var b,
                    AbsBody:
                    {
                        Type: ExprType.App,
                        AppLeft:
                        {
                            Type: ExprType.App,
                            AppLeft: { Type: ExprType.Var, VarName: var p2 },
                            AppRight: { Type: ExprType.Var, VarName: var a2 }
                        },
                        AppRight: { Type: ExprType.Var, VarName: var b2 }
                    }
                }
            }
        } && p == p2 && a == a2 && b == b2;
}
