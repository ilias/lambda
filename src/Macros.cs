namespace LambdaCalculus;

// Macro system components
public enum MacroPatternType : byte { Literal, Variable, List }

public abstract record MacroPattern(MacroPatternType Type)
{
    public static MacroPattern Literal(string value) => new LiteralPattern(value);
    public static MacroPattern Variable(string name) => new VariablePattern(name);
    public static MacroPattern List(IList<MacroPattern> patterns) => new ListPattern(patterns);
}

public record LiteralPattern(string Value) : MacroPattern(MacroPatternType.Literal);
public record VariablePattern(string Name) : MacroPattern(MacroPatternType.Variable);
public record ListPattern(IList<MacroPattern> Patterns) : MacroPattern(MacroPatternType.List);

public record MacroDefinition(string Name, IList<MacroPattern> Pattern, Expr Transformation)
{
    public override string ToString() =>
        $":macro ({Name} {string.Join(" ", Pattern.Select(FormatPattern))}) => {Transformation}"
        .Replace("__MACRO_VAR_", "$")
        .Replace("__MACRO_INT_", "");
    
    private static string FormatPattern(MacroPattern pattern) => pattern switch
    {
        LiteralPattern lit => lit.Value,
        VariablePattern var => $"${var.Name}",
        ListPattern list => $"({string.Join(" ", list.Patterns.Select(FormatPattern))})",
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