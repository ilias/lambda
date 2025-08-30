namespace LambdaCalculus;

// Macro system components
public enum MacroPatternType : byte { Literal, Variable, List, Wildcard, IntLiteral }

public abstract record MacroPattern(MacroPatternType Type)
{
    public static MacroPattern Literal(string value) => new LiteralPattern(value);
    public static MacroPattern Variable(string name, bool isRest = false) => new VariablePattern(name, isRest);
    public static MacroPattern List(IList<MacroPattern> patterns) => new ListPattern(patterns);
    public static MacroPattern Wildcard() => new WildcardPattern();
    public static MacroPattern IntLiteral(int value) => new IntLiteralPattern(value);
}

public record LiteralPattern(string Value) : MacroPattern(MacroPatternType.Literal);
public record VariablePattern(string Name, bool IsRest) : MacroPattern(MacroPatternType.Variable);
public record ListPattern(IList<MacroPattern> Patterns) : MacroPattern(MacroPatternType.List);
public record WildcardPattern() : MacroPattern(MacroPatternType.Wildcard);
public record IntLiteralPattern(int Value) : MacroPattern(MacroPatternType.IntLiteral);

// A single macro clause (pattern -> transformation). Multiple clauses with same Name allowed.
public record MacroDefinition(string Name, IList<MacroPattern> Pattern, Expr Transformation, Expr? Guard = null)
{
    public override string ToString() =>
    $":macro ({Name} {string.Join(" ", Pattern.Select(FormatPattern))})" +
    (Guard is not null ? $" when {Guard}" : "") +
    $" => {Transformation}"
        .Replace("__MACRO_VAR_", "$")
        .Replace("__MACRO_INT_", "");

    private static string FormatPattern(MacroPattern pattern) => pattern switch
    {
        LiteralPattern lit => lit.Value,
    VariablePattern var => var.IsRest ? $"${var.Name}..." : $"${var.Name}",
        ListPattern list => $"({string.Join(" ", list.Patterns.Select(FormatPattern))})",
    WildcardPattern => "_",
    IntLiteralPattern lit => lit.Value.ToString(),
        _ => "?"
    };
}

public class MacroExpansionResult
{
    public bool Success { get; init; }
    public Expr? ExpandedExpr { get; init; }
    public string? ErrorMessage { get; init; }
    
    public static MacroExpansionResult Successful(Expr expr) => new() { Success = true, ExpandedExpr = expr };
    public static MacroExpansionResult Failed(string error) => new() { Success = false, ErrorMessage = error };
}