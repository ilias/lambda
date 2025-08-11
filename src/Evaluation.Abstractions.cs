namespace LambdaCalculus;

/// <summary>
/// Abstraction for evaluation so different strategies (CEK, closure-based, graph-reduction) can coexist.
/// </summary>
public interface IEvaluator
{
    Expr Evaluate(Expr expr, Dictionary<string, Expr>? initialEnv = null);
}

/// <summary>
/// Current CEK-machine based evaluator (wrapper around existing Interpreter implementation).
/// </summary>
internal sealed class CEKEvaluator : IEvaluator
{
    private readonly Interpreter _interpreter;
    public CEKEvaluator(Interpreter interpreter) => _interpreter = interpreter;
    public Expr Evaluate(Expr expr, Dictionary<string, Expr>? initialEnv = null)
        => _interpreter.EvaluateCEK(expr, initialEnv);
}

/// <summary>
/// Placeholder for an upcoming closure-based direct evaluator prototype.
/// Will construct explicit Closure objects capturing environment instead of
/// re-building env dictionaries each step, enabling alternative reduction order.
/// </summary>
internal sealed class ClosureEvaluator : IEvaluator
{
    public Expr Evaluate(Expr expr, Dictionary<string, Expr>? initialEnv = null)
        => throw new NotImplementedException("Closure-based evaluator prototype not yet implemented");
}
