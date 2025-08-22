namespace LambdaCalculus;

// Cache key for substitution optimization with pre-computed hash.
public readonly record struct SubstitutionCacheKey(Expr Root, string VarName, Expr Value)
{
    private readonly int _hashCode = unchecked(
        (Root?.GetHashCode() ?? 0) * 397 ^
        (VarName?.GetHashCode() ?? 0) * 31 ^
        (Value?.GetHashCode() ?? 0)
    );
    public override int GetHashCode() => _hashCode;
}

public enum KontinuationType : byte { Empty, Arg, Fun, Conditional }
public record Kontinuation(KontinuationType Type, Expr? Expression = null,
    Dictionary<string, Expr>? Environment = null, Expr? Value = null, Kontinuation? Next = null,
    string? VarName = null, Expr? ThenBranch = null, Expr? ElseBranch = null)
{
    public static readonly Kontinuation Empty = new(KontinuationType.Empty);

    public static Kontinuation Arg(Expr expr, Dictionary<string, Expr> env, Kontinuation next) =>
        new(KontinuationType.Arg, expr, env, Next: next);

    public static Kontinuation Fun(Expr value, Kontinuation next) =>
        new(KontinuationType.Fun, Value: value, Next: next);

    public static Kontinuation Conditional(Expr thenBranch, Expr elseBranch, Dictionary<string, Expr> env, Kontinuation next) =>
        new(KontinuationType.Conditional, Environment: env, Next: next, ThenBranch: thenBranch, ElseBranch: elseBranch);
}
