namespace LambdaCalculus;

public enum ExprType : byte { Var, Abs, App, Thunk, YCombinator }

// Thunk represents a delayed computation (mutable for in-place update).
public class Thunk(Expr expr, Dictionary<string, Expr> env)
{
    public Expr Expression { get; } = expr;
    public Dictionary<string, Expr> Environment { get; } = env;
    public bool IsForced { get; private set; }
    public bool IsBeingForced { get; private set; }
    public Expr? ForcedValue { get; private set; }

    public void Force(Expr value)
    {
        IsForced = true;
        ForcedValue = value;
        IsBeingForced = false;
    }

    public void BeginForce() => IsBeingForced = true;
}

public record Expr(
    ExprType Type,
    string? VarName = null,
    string? AbsVarName = null, Expr? AbsBody = null,
    Expr? AppLeft = null, Expr? AppRight = null,
    Thunk? ThunkValue = null)
{
    public static int HashCodeCount { get; private set; }
    private int? _hashCode;
    public static Expr Var(string name) => new(ExprType.Var, VarName: name);
    public static Expr Abs(string name, Expr body) => new(ExprType.Abs, AbsVarName: name, AbsBody: body);
    public static Expr App(Expr left, Expr right) => new(ExprType.App, AppLeft: left, AppRight: right);
    public static Expr Thunk(Expr expr, Dictionary<string, Expr> env) => new(ExprType.Thunk, ThunkValue: new Thunk(expr, env));
    public static Expr YCombinator() => new(ExprType.YCombinator);
    
    public override string ToString() => ToString(false, null);
    
    public string ToString(bool prettyPrint, Func<Expr, int?>? churchNumeralExtractor = null)
    {
        var result = ToStringWithOptions(1000, [], prettyPrint, churchNumeralExtractor);
        return result.Length <= 5000 ? result : result[..5000] + "... (output truncated)";
    }

    private string ToStringWithOptions(int maxDepth, HashSet<Expr> visited, bool prettyPrint, Func<Expr, int?>? churchNumeralExtractor)
    {
        if (maxDepth <= 0) return "...";
        if (visited.Contains(this)) return "<cycle>";

        // Check for Church numeral formatting if enabled
        if (prettyPrint && churchNumeralExtractor != null)
        {
            var number = churchNumeralExtractor(this);
            if (number.HasValue)
                return number.Value.ToString();
        }

        // List pretty-printing: [a, b, c] (only if formatNumerals is enabled)
        if (prettyPrint)
        {
            // 0. Church booleans
            if (TryExtractBoolean(this, out var boolVal))
                return boolVal ? "true" : "false";
            // 1. cons/nil lists
            if (Expr.TryExtractListElements(this, out var elements))
            {
                var elemsStr = string.Join(", ", elements.Select(e => e.ToStringWithOptions(maxDepth - 1, visited, prettyPrint, churchNumeralExtractor)));
                return "[" + elemsStr + "]";
            }
            // 2. Church-encoded lists: λf.λz.f a1 (f a2 (... (f an z)...))
            if (Expr.TryExtractChurchListElements(this, out var chElems, churchNumeralExtractor))
            {
                var elemsStr = string.Join(", ", chElems.Select(e => e.ToStringWithOptions(maxDepth - 1, visited, prettyPrint, churchNumeralExtractor)));
                return "[" + elemsStr + "]";
            }
        }

        // Fallback: default pretty-printing for all other cases
        visited.Add(this);
        try
        {
            return Type switch
            {
                ExprType.Var => VarName!,
                ExprType.Abs => $"λ{AbsVarName}.{AbsBody?.ToStringWithOptions(maxDepth - 1, visited, prettyPrint, churchNumeralExtractor) ?? "null"}",
                ExprType.App => FormatApplicationWithOptions(maxDepth, visited, prettyPrint, churchNumeralExtractor),
                ExprType.Thunk => ThunkValue!.IsForced ?
                    $"<forced:{ThunkValue.ForcedValue?.ToStringWithOptions(maxDepth - 1, visited, prettyPrint, churchNumeralExtractor) ?? "null"}>" :
                    $"<thunk:{ThunkValue.Expression.ToStringWithOptions(maxDepth - 1, visited, prettyPrint, churchNumeralExtractor)}>",
                ExprType.YCombinator => "Y",
                _ => "?"
            };
        }
        finally
        {
            visited.Remove(this);
        }
    }

    // Recognize cons a b as App(App(Var("cons"), a), b) and nil as Var("nil")
    public static bool TryExtractListElements(Expr expr, out List<Expr> elements)
    {
        elements = [];
        var cur = expr;
        while (IsCons(cur, out var head, out var tail))
        {
            elements.Add(head);
            cur = tail;
        }
        if (IsNil(cur))
            return true;
        elements.Clear();
        return false;
    }

    // Try to extract elements from a Church-encoded list λf.λz.f a1 (f a2 (... (f an z)...))
    public static bool TryExtractChurchListElements(Expr expr, out List<Expr> elements, Func<Expr, int?>? churchNumeralExtractor)
    {
        elements = [];
        if (expr.Type != ExprType.Abs || expr.AbsBody == null) return false;
        var fvar = expr.AbsVarName;
        var body = expr.AbsBody;
        if (body.Type != ExprType.Abs || body.AbsBody == null) return false;
        var zvar = body.AbsVarName;
        var cur = body.AbsBody;
        
        while (cur.Type == ExprType.App && 
               cur.AppLeft?.Type == ExprType.App && 
               cur.AppLeft.AppLeft?.Type == ExprType.Var &&
               cur.AppLeft.AppLeft.VarName == fvar)
        {
            elements.Add(cur.AppLeft.AppRight!);
            cur = cur.AppRight!;
        }
        
        // If we end with the z variable, it's a valid Church-encoded list
        if (cur.Type == ExprType.Var && cur.VarName == zvar)
            return true;
            
        // Not a valid Church-encoded list
        elements.Clear();
        return false;
    }


    // Recognize cons a b as App(App(Var("cons"), a), b)
    private static bool IsCons(Expr expr, out Expr head, out Expr tail)
    {
        head = tail = null!;
        if (expr.Type == ExprType.App && expr.AppLeft is { Type: ExprType.App, AppLeft: { Type: ExprType.Var, VarName: "cons" }, AppRight: var h } && expr.AppRight is var t)
        {
            head = h!;
            tail = t!;
            return true;
        }
        return false;
    }

    // Recognize nil as Var("nil")
    private static bool IsNil(Expr expr)
        => expr.Type == ExprType.Var && expr.VarName == "nil";

    private string FormatApplicationWithOptions(int maxDepth, HashSet<Expr> visited, bool formatNumerals, Func<Expr, int?>? churchNumeralExtractor)
    {
        var isLeftANumber = formatNumerals && churchNumeralExtractor != null && churchNumeralExtractor(AppLeft!) != null;
        var needsParens = AppLeft!.Type == ExprType.Abs && !isLeftANumber;
        
        var leftStr = AppLeft.ToStringWithOptions(maxDepth - 1, visited, formatNumerals, churchNumeralExtractor);
        if (needsParens)
            leftStr = $"({leftStr})";

        var isRightANumber = formatNumerals && churchNumeralExtractor != null && churchNumeralExtractor(AppRight!) != null;
        var needsParens2 = AppRight!.Type is ExprType.App or ExprType.Abs && !isRightANumber;
        
        var rightStr = AppRight.ToStringWithOptions(maxDepth - 1, visited, formatNumerals, churchNumeralExtractor);
        if (needsParens2)
            rightStr = $"({rightStr})";

        return $"{leftStr} {rightStr}";
    }
    public virtual bool Equals(Expr? other) => StructuralEquals(other);
    // Detect Church booleans: true = λa.λb.a, false = λa.λb.b
    public static bool TryExtractBoolean(Expr expr, out bool value)
    {
        value = false;
        if (expr.Type != ExprType.Abs || expr.AbsBody is null) return false;
        var a = expr.AbsVarName;
        var body = expr.AbsBody;
        if (body.Type != ExprType.Abs || body.AbsBody is null) return false;
        var b = body.AbsVarName;
        var inner = body.AbsBody;
        if (inner.Type != ExprType.Var || inner.VarName is null) return false;
        if (inner.VarName == a)
        {
            value = true; // λa.λb.a
            return true;
        }
        if (inner.VarName == b)
        {
            value = false; // λa.λb.b
            return true;
        }
        return false;
    }
    public bool StructuralEquals(Expr? other)
    {
        if (ReferenceEquals(this, other)) return true;
        if (other is null || Type != other.Type) return false;
        var stack = new Stack<(Expr Left, Expr Right)>();
        stack.Push((this, other));
        while (stack.Count > 0)
        {
            var (left, right) = stack.Pop();
            if (left.Type != right.Type) return false;
            if (ReferenceEquals(left, right)) continue;
            switch (left.Type)
            {
                case ExprType.Var when left.VarName != right.VarName: return false;
                case ExprType.Var: break;
                case ExprType.Abs when left.AbsVarName != right.AbsVarName: return false;
                case ExprType.Abs when left.AbsBody is null && right.AbsBody is null: continue;
                case ExprType.Abs when left.AbsBody is null || right.AbsBody is null: return false;
                case ExprType.Abs:
                    stack.Push((left.AbsBody!, right.AbsBody!));
                    break;
                case ExprType.App when AreApplicationsEmpty(left, right): continue;
                case ExprType.App when HasMissingApplicationParts(left, right): return false;
                case ExprType.App:
                    stack.Push((left.AppRight!, right.AppRight!));
                    stack.Push((left.AppLeft!, right.AppLeft!));
                    break;
                case ExprType.Thunk when left.ThunkValue is null && right.ThunkValue is null: continue;
                case ExprType.Thunk when left.ThunkValue is null || right.ThunkValue is null: return false;
                case ExprType.Thunk:
                    if (left.ThunkValue!.IsForced && right.ThunkValue!.IsForced)
                        stack.Push((left.ThunkValue.ForcedValue!, right.ThunkValue.ForcedValue!));
                    else if (!left.ThunkValue.IsForced && !right.ThunkValue.IsForced)
                        stack.Push((left.ThunkValue.Expression, right.ThunkValue.Expression));
                    else
                        return false;
                    break;
                case ExprType.YCombinator: break;
                default: return false;
            }
        }
        return true;
    }
    private static bool AreApplicationsEmpty(Expr left, Expr right) =>
        left.AppLeft is null && left.AppRight is null &&
        right.AppLeft is null && right.AppRight is null;
        
    private static bool HasMissingApplicationParts(Expr left, Expr right) =>
        left.AppLeft is null || left.AppRight is null ||
        right.AppLeft is null || right.AppRight is null;
    public override int GetHashCode()
    {
        if (_hashCode.HasValue) return _hashCode.Value;
        var stack = new Stack<Expr>();
        stack.Push(this);
        while (stack.Count > 0)
        {
            HashCodeCount++;
            var current = stack.Pop();
            if (current._hashCode.HasValue) continue;
            switch (current.Type)
            {
                case ExprType.Var:
                    current._hashCode = HashCode.Combine(current.Type, current.VarName);
                    break;
                case ExprType.Abs when current.AbsBody is null || current.AbsBody._hashCode.HasValue:
                    current._hashCode = HashCode.Combine(current.Type, current.AbsVarName,
                        current.AbsBody?._hashCode ?? 0);
                    break;
                case ExprType.Abs:
                    stack.Push(current);
                    stack.Push(current.AbsBody!);
                    break;
                case ExprType.App when BothApplicationPartsHaveHashCodes(current):
                    current._hashCode = HashCode.Combine(current.Type,
                        current.AppLeft!._hashCode ?? 0, current.AppRight!._hashCode ?? 0);
                    break;
                case ExprType.App:
                    stack.Push(current);
                    if (current.AppLeft?._hashCode is null) stack.Push(current.AppLeft!);
                    if (current.AppRight?._hashCode is null) stack.Push(current.AppRight!);
                    break;
                case ExprType.Thunk when current.ThunkValue is not null:
                    var thunkHash = current.ThunkValue.IsForced && current.ThunkValue.ForcedValue is not null
                        ? current.ThunkValue.ForcedValue.GetHashCode()
                        : current.ThunkValue.Expression.GetHashCode();
                    current._hashCode = HashCode.Combine(current.Type, thunkHash);
                    break;
                case ExprType.YCombinator:
                    current._hashCode = HashCode.Combine(current.Type);
                    break;
            }
        }
        return _hashCode ?? 0;
    }
    private static bool BothApplicationPartsHaveHashCodes(Expr expr) =>
        expr.AppLeft?._hashCode is not null && expr.AppRight?._hashCode is not null;
}

public readonly struct ExprEqualityComparer : IEqualityComparer<Expr>
{
    public bool Equals(Expr? x, Expr? y) => (x, y) switch
    {
        (null, null) => true,
        ({ } a, { } b) => a.StructuralEquals(b),
        _ => false
    };

    public int GetHashCode(Expr obj) => obj?.GetHashCode() ?? 0;
}