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
public record CEKState(Expr Control, Dictionary<string, Expr> Environment, Kontinuation Kontinuation);

public enum TokenType : byte { LParen, RParen, Lambda, Term, Equals, Integer, LBracket, RBracket, Comma, Dot, Y, Let, In, Rec, InfixOp, Arrow, Range, Macro, FatArrow, Dollar }
public record Token(TokenType Type, int Position, string? Value = null);
public enum TreeErrorType : byte { UnclosedParen, UnopenedParen, MissingLambdaVar, MissingLambdaBody, EmptyExprList, IllegalAssignment, MissingLetVariable, MissingLetEquals, MissingLetIn, MissingLetValue, MissingLetBody }
public class ParseException(TreeErrorType errorType, int position)
    : Exception($"{errorType} at position {position}")
{
    public TreeErrorType ErrorType { get; } = errorType;
    public int Position { get; } = position;
}

public enum StatementType : byte { Expr, Assignment }
public record Statement(StatementType Type, Expr Expression, string? VarName = null)
{
    public static Statement ExprStatement(Expr expr) =>
        new(StatementType.Expr, expr);

    public static Statement AssignmentStatement(string varName, Expr expr) =>
        new(StatementType.Assignment, expr, varName);

    public override string ToString() => Type == StatementType.Expr
        ? Expression.ToString()
        : $"{VarName} = {Expression}";
}

// Tracks interpreter statistics and performance metrics.
public class InterpreterStats
{
    public long TimeInCacheLookup { get; set; }
    public long TimeInSubstitution { get; set; }
    public long TimeInEvaluation { get; set; }
    public long TimeInForcing { get; set; }
    public int NormalizeCEKCount { get; set; }
    public int CacheHits { get; set; }
    public int CacheMisses { get; set; }
    public int TotalIterations { get; set; }
    public int Iterations { get; set; }
    public int SubstitutionExprCount { get; set; }
    public int ThunkForceCount { get; set; }
    public int VarCounter { get; set; }
    public int MaxRecursionDepth { get; set; } = 20;
    public void Reset()
    {
        TimeInCacheLookup = 0;
        TimeInSubstitution = 0;
        TimeInEvaluation = 0;
        TimeInForcing = 0;
        NormalizeCEKCount = 0;
        CacheHits = 0;
        CacheMisses = 0;
        TotalIterations = 0;
        Iterations = 0;
        SubstitutionExprCount = 0;
        ThunkForceCount = 0;
        VarCounter = 0;
    }
}

// Represents an infix operator with precedence and associativity
public enum Associativity : byte { Left, Right }
public record InfixOperator(string Symbol, int Precedence, Associativity Associativity, string? FunctionName = null)
{
    public string GetFunctionName() => FunctionName ?? Symbol;
}

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
    public override string ToString() => $":macro ({Name} {string.Join(" ", Pattern.Select(FormatPattern))}) => {Transformation}";
    
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

public class Parser
{
    public readonly Dictionary<string, InfixOperator> _infixOperators = new(StringComparer.Ordinal);
    public readonly Dictionary<string, MacroDefinition> _macros = new(StringComparer.Ordinal);

    public Parser()
    {
        // Register pipeline operator as right-associative, low precedence
        DefineInfixOperator("|>", 1, "left");
        // Register composition operator '.' as right-associative, high precedence
        DefineInfixOperator(".", 9, "right");
    }
    public string DefineInfixOperator(string symbol, int precedence, string associativity)
    {
        if (string.IsNullOrWhiteSpace(symbol))
            return "Error: Operator symbol cannot be empty";
        
        if (precedence is < 1 or > 10)
            return "Error: Precedence must be between 1 and 10";
        
        if (!Enum.TryParse<Associativity>(associativity, true, out var assoc))
            return "Error: Associativity must be 'left' or 'right'";
        
        _infixOperators[symbol] = new(symbol, precedence, assoc);
        return $"Infix operator '{symbol}' defined with precedence {precedence} and {associativity} associativity";
    }

    public bool IsInfixOperator(string symbol) => _infixOperators.ContainsKey(symbol);
    public InfixOperator? GetInfixOperator(string symbol) => _infixOperators.GetValueOrDefault(symbol);

    TokenType MyTokenType(string? term) => term switch
    {
        "Y" => TokenType.Y,
        "let" => TokenType.Let,
        "in" => TokenType.In,
        "rec" => TokenType.Rec,
        ":macro" => TokenType.Macro,
        _ when term != null && IsInfixOperator(term) => TokenType.InfixOp,
        _ => TokenType.Term
    }; 

    public List<Token> Tokenize(string input)
    {
        if (string.IsNullOrWhiteSpace(input)) return [];

        var result = new List<Token>();
        var currentTerm = new System.Text.StringBuilder();
        var pos = 0;

        for (var i = 0; i < input.Length; i++)
        {
            var ch = input[i];
            pos++;
            if (ch == '#') break; // Comments

            // Check for arrow operators first
            var remainingInput = input.AsSpan(i);
            if (remainingInput.StartsWith("=>"))
            {
                if (currentTerm.Length > 0)
                {
                    var termValue = currentTerm.ToString();
                    var tokenType = MyTokenType(termValue);
                    result.Add(new Token(tokenType, pos - currentTerm.Length, termValue));
                    currentTerm.Clear();
                }
                
                result.Add(new Token(TokenType.FatArrow, pos));
                i += 1; // Skip the next character too
                pos += 1;
                continue;
            }
            if (remainingInput.StartsWith("->"))
            {
                if (currentTerm.Length > 0)
                {
                    var termValue = currentTerm.ToString();
                    var tokenType = MyTokenType(termValue);
                    result.Add(new Token(tokenType, pos - currentTerm.Length, termValue));
                    currentTerm.Clear();
                }
                
                result.Add(new Token(TokenType.Arrow, pos));
                i += 1; // Skip the next character too
                pos += 1;
                continue;
            }

            // Improved: treat the first '.' after a lambda (λ or \\) and its parameters as a lambda body separator, not as infix
            string? bestMatch = null;
            bool isLambdaBodyDot = false;
            // Special handling for range operator '..'
            if (ch == '.' && i + 1 < input.Length && input[i + 1] == '.') {
                if (currentTerm.Length > 0) {
                    var termValue = currentTerm.ToString();
                    var tokenType = MyTokenType(termValue);
                    result.Add(new Token(tokenType, pos - currentTerm.Length, termValue));
                    currentTerm.Clear();
                }
                result.Add(new Token(TokenType.Range, pos, ".."));
                i++;
                pos++;
                continue;
            }
            if (ch == '.') {
                // Look back through the tokens we've already produced
                int t = result.Count - 1;
                bool foundDotOrAssign = false;
                while (t >= 0) {
                    var tok = result[t];
                    if (tok.Type == TokenType.Dot || tok.Type == TokenType.Equals) 
                        break;
                    if (tok.Type == TokenType.Lambda) {
                        if (!foundDotOrAssign) {
                            isLambdaBodyDot = true;
                        }
                        break;
                    }
                    // Skip over identifiers, integers, etc.
                    if (tok.Type == TokenType.Term || tok.Type == TokenType.Integer) {
                        t--;
                        continue;
                    }
                    // For all other tokens, stop
                    break;
                }
            }
            // Always treat the first dot after a lambda and its parameters as TokenType.Dot (lambda body separator)
            if (ch == '.' && isLambdaBodyDot) {
                if (currentTerm.Length > 0) {
                    var termValue = currentTerm.ToString();
                    var tokenType = MyTokenType(termValue);
                    result.Add(new Token(tokenType, pos - currentTerm.Length, termValue));
                    currentTerm.Clear();
                }
                result.Add(new Token(TokenType.Dot, pos));
                continue;
            }
            if (!char.IsLetterOrDigit(ch) && !char.IsWhiteSpace(ch) && !(ch == '.' && isLambdaBodyDot))
            {
                foreach (var opSymbol in _infixOperators.Keys)
                {
                    if (remainingInput.StartsWith(opSymbol) &&
                        (bestMatch is null || opSymbol.Length > bestMatch.Length))
                            bestMatch = opSymbol;
                }
            }

            if (bestMatch != null)
            {
                if (currentTerm.Length > 0)
                {
                    var termValue = currentTerm.ToString();
                    var tokenType = MyTokenType(termValue);
                    result.Add(new Token(tokenType, pos - currentTerm.Length, termValue));
                    currentTerm.Clear();
                }
                result.Add(new Token(TokenType.InfixOp, pos, bestMatch));
                i += bestMatch.Length - 1;
                pos += bestMatch.Length - 1;
                continue;
            }

            // Only treat '=' as TokenType.Equals when it is the only character in the token
            Token? nextToken = ch switch
            {
                '\\' or 'λ' => new Token(TokenType.Lambda, pos),
                '(' => new Token(TokenType.LParen, pos),
                ')' => new Token(TokenType.RParen, pos),
                '[' => new Token(TokenType.LBracket, pos),
                ']' => new Token(TokenType.RBracket, pos),
                ',' => new Token(TokenType.Comma, pos),
                '$' => new Token(TokenType.Dollar, pos),
                '=' when currentTerm.Length == 0 && (i + 1 == input.Length || !input[i + 1].Equals('=')) => new Token(TokenType.Equals, pos),
                '.' => new Token(TokenType.Dot, pos),
                char c when char.IsDigit(c) && currentTerm.Length == 0 => ParseInteger(input, ref i, ref pos),
                _ => null
            };

            // If '=' is not the only character, treat it as part of a term (e.g., '==')
            if (ch == '=' && nextToken is null && !char.IsWhiteSpace(ch))
            {
                currentTerm.Append(ch);
                continue;
            }

            if (nextToken is null && !char.IsWhiteSpace(ch))
                currentTerm.Append(ch);
            else
            {
                if (currentTerm.Length > 0)
                {
                    var termValue = currentTerm.ToString();
                    var tokenType = MyTokenType(termValue);
                    result.Add(new Token(tokenType, pos - currentTerm.Length, termValue));
                    currentTerm.Clear();
                }
                if (nextToken is not null)
                    result.Add(nextToken);
            }
        }

        if (currentTerm.Length > 0)
        {
            var termValue = currentTerm.ToString();
            var tokenType = MyTokenType(termValue);
            result.Add(new Token(tokenType, pos - currentTerm.Length + 1, termValue));
        }

        return result;
    }
    private Token ParseInteger(string input, ref int i, ref int pos)
    {
        var start = i;
        var startPos = pos;
        while (i + 1 < input.Length && char.IsDigit(input[i + 1]))
        {
            pos++;
            i++;
        }
        return new Token(TokenType.Integer, startPos, input[start..(i + 1)]);
    }
    public Statement? Parse(string input)
    {
        var tokens = Tokenize(input);
        if (tokens.Count == 0) return null;

        // Macro definition: :macro (name pattern...) => transformation
        if (tokens.Count > 0 && tokens[0].Type == TokenType.Macro)
        {
            var macro = ParseMacroDefinition(tokens);
            _macros[macro.Name] = macro;
            return null; // Macro definitions don't produce statements
        }

        // Assignment: name = expr
        if (tokens.Count > 2 && tokens[0].Type == TokenType.Term && tokens[1].Type == TokenType.Equals)
        {
            var assignmentExpr = BuildExpressionTree(tokens, 2, tokens.Count - 1);
            var expandedAssignmentExpr = ExpandMacros(assignmentExpr);
            return Statement.AssignmentStatement(tokens[0].Value!, expandedAssignmentExpr);
        }

        // Let expression: let ... in ...
        if (tokens.Count > 0 && tokens[0].Type == TokenType.Let)
        {
            var i = 0;
            var letExpr = ParseLetExpr(tokens, ref i, tokens.Count - 1);
            return Statement.ExprStatement(letExpr);
        }

        // Expression statement with macro expansion
        var expr = BuildExpressionTree(tokens, 0, tokens.Count - 1);
        var expandedExpr = ExpandMacros(expr);
        return Statement.ExprStatement(expandedExpr);
    }
    private Expr BuildExpressionTree(List<Token> tokens, int start, int end)
    {
        // Handle arrow functions first (they have lowest precedence)
        if (HasArrowFunction(tokens, start, end))
            return ParseArrowFunction(tokens, start, end);
            
        // Handle infix expressions using the Shunting Yard algorithm
        if (HasInfixOperators(tokens, start, end))
            return ParseInfixExpression(tokens, start, end);

        var expressions = new List<Expr>();
        for (var i = start; i <= end; i++)
        {
            var token = tokens[i];
            expressions.Add(token.Type switch
            {
                TokenType.LParen => ParseParenthesizedExpr(tokens, ref i, end),
                TokenType.RParen => throw new ParseException(TreeErrorType.UnopenedParen, token.Position),
                TokenType.LBracket => ParseListExpr(tokens, ref i, end),
                TokenType.RBracket => throw new ParseException(TreeErrorType.UnopenedParen, token.Position),
                TokenType.Lambda => ParseLambdaExpr(tokens, ref i, end),
                TokenType.Let => ParseLetExpr(tokens, ref i, end),
                TokenType.Y => Expr.YCombinator(),
                TokenType.Term => Expr.Var(token.Value!),
                TokenType.Integer when int.TryParse(token.Value, out int value) => CreateChurchNumeral(value),
                TokenType.InfixOp => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position), // Should be handled by infix parser
                TokenType.Arrow => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position), // Should be handled by arrow parser
                TokenType.Equals => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position),
                _ => throw new ParseException(TreeErrorType.EmptyExprList, token.Position)
            });
        }
        return expressions.Count == 0
            ? throw new ParseException(TreeErrorType.EmptyExprList, tokens.Count > 0 ? tokens[0].Position : 0)
            : expressions.Aggregate(Expr.App);
    }

    private bool HasInfixOperators(List<Token> tokens, int start, int end)
    {
        var nesting = 0;
        for (var i = start; i <= end; i++)
        {
            var token = tokens[i];
            if (token.Type == TokenType.LParen || token.Type == TokenType.LBracket) nesting++;
            else if (token.Type == TokenType.RParen || token.Type == TokenType.RBracket) nesting--;
            else if (nesting == 0 && token.Type == TokenType.InfixOp) return true;
        }
        return false;
    }

    private bool HasArrowFunction(List<Token> tokens, int start, int end)
    {
        var nesting = 0;
        for (var i = start; i <= end; i++)
        {
            var token = tokens[i];
            if (token.Type == TokenType.LParen || token.Type == TokenType.LBracket) nesting++;
            else if (token.Type == TokenType.RParen || token.Type == TokenType.RBracket) nesting--;
            else if (nesting == 0 && token.Type == TokenType.Arrow) return true;
            // Stop at keywords that should not be part of an arrow function
            else if (nesting == 0 && (token.Type == TokenType.In || token.Type == TokenType.Let)) break;
        }
        return false;
    }

    private Expr ParseArrowFunction(List<Token> tokens, int start, int end)
    {
        // Find the leftmost arrow at the top level (arrows are right-associative)
        // For right-associativity, we parse from left to right: x -> y -> z becomes x -> (y -> z)
        var arrowPos = -1;
        var nesting = 0;
        
        for (var i = start; i <= end; i++)
        {
            var token = tokens[i];
            if (token.Type == TokenType.LParen || token.Type == TokenType.LBracket) nesting++;
            else if (token.Type == TokenType.RParen || token.Type == TokenType.RBracket) nesting--;
            else if (nesting == 0 && token.Type == TokenType.Arrow)
            {
                arrowPos = i;
                break; // Take the first (leftmost) arrow for right-associativity
            }
            // Stop at keywords that should not be part of an arrow function
            else if (nesting == 0 && (token.Type == TokenType.In || token.Type == TokenType.Let)) break;
        }
        
        if (arrowPos == -1 || arrowPos == start || arrowPos == end)
            throw new ParseException(TreeErrorType.IllegalAssignment, tokens[start].Position);
        
        // Find the actual end of the arrow function body
        // It should stop at keywords like 'in' or 'let' at the top level
        var bodyEnd = end;
        var nesting2 = 0;
        for (var i = arrowPos + 1; i <= end; i++)
        {
            var token = tokens[i];
            if (token.Type == TokenType.LParen || token.Type == TokenType.LBracket) nesting2++;
            else if (token.Type == TokenType.RParen || token.Type == TokenType.RBracket) nesting2--;
            else if (nesting2 == 0 && (token.Type == TokenType.In || token.Type == TokenType.Let))
            {
                bodyEnd = i - 1;
                break;
            }
        }
        
        // Check if the left side is a simple parameter list (only terms and commas)
        var isSimpleParamList = true;
        for (var i = start; i < arrowPos; i++)
        {
            var token = tokens[i];
            if (token.Type != TokenType.Term && token.Type != TokenType.Comma)
            {
                isSimpleParamList = false;
                break;
            }
        }
        
        if (isSimpleParamList)
        {
            // Parse parameter list (left side of arrow)
            var paramTokens = new List<string>();
            for (var i = start; i < arrowPos; i++)
            {
                var token = tokens[i];
                if (token.Type == TokenType.Term)
                    paramTokens.Add(token.Value!);
                else if (token.Type != TokenType.Comma)
                    throw new ParseException(TreeErrorType.IllegalAssignment, token.Position);
            }
            
            if (paramTokens.Count == 0)
                throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[start].Position);
            
            // Parse body (right side of arrow)
            var body = BuildExpressionTree(tokens, arrowPos + 1, bodyEnd);
            
            // Build nested lambdas from right to left (innermost to outermost)
            return paramTokens.AsEnumerable().Reverse()
                .Aggregate(body, (expr, param) => Expr.Abs(param, expr));
        }
        else
        {
            // Left side is not a simple parameter list but could be a single term
            // For cases like "x -> y -> x + y", left side is "x" (single term)
            if (arrowPos == start + 1 && tokens[start].Type == TokenType.Term)
            {
                // Single parameter case: "x -> (body)"
                var param = tokens[start].Value!;
                var body = BuildExpressionTree(tokens, arrowPos + 1, bodyEnd);
                return Expr.Abs(param, body);
            }
            else
            {
                // Left side is complex expression, which is not valid for arrow functions
                throw new ParseException(TreeErrorType.IllegalAssignment, tokens[start].Position);
            }
        }
    }

    private Expr ParseInfixExpression(List<Token> tokens, int start, int end)
    {
        var outputQueue = new Queue<object>();
        var operatorStack = new Stack<InfixOperator>();

        for (var i = start; i <= end; i++)
        {
            var token = tokens[i];

            switch (token.Type)
            {
                case TokenType.LParen:
                    var parenExpr = ParseParenthesizedExpr(tokens, ref i, end);
                    outputQueue.Enqueue(parenExpr);
                    break;

                case TokenType.LBracket:
                    var listExpr = ParseListExpr(tokens, ref i, end);
                    outputQueue.Enqueue(listExpr);
                    break;

                case TokenType.Lambda:
                    var lambdaExpr = ParseLambdaExpr(tokens, ref i, end);
                    outputQueue.Enqueue(lambdaExpr);
                    break;

                case TokenType.Let:
                    var letExpr = ParseLetExpr(tokens, ref i, end);
                    outputQueue.Enqueue(letExpr);
                    break;

                case TokenType.Y:
                    outputQueue.Enqueue(Expr.YCombinator());
                    break;

                case TokenType.Term:
                    outputQueue.Enqueue(Expr.Var(token.Value!));
                    break;

                case TokenType.Integer when int.TryParse(token.Value, out int value):
                    outputQueue.Enqueue(CreateChurchNumeral(value));
                    break;

                case TokenType.InfixOp:
                    var currentOp = GetInfixOperator(token.Value!)!;

                    while (operatorStack.Count > 0)
                    {
                        var topOp = operatorStack.Peek();
                        // For left-associative: pop while (top > current) or (top == current && left-assoc)
                        // For right-associative: pop while (top > current)
                        if (topOp.Precedence > currentOp.Precedence ||
                            (topOp.Precedence == currentOp.Precedence && topOp.Associativity == Associativity.Left))
                        {
                            outputQueue.Enqueue(operatorStack.Pop());
                        }
                        else break;
                    }

                    operatorStack.Push(currentOp);
                    break;
            }
        }

        // Pop remaining operators
        while (operatorStack.Count > 0)
            outputQueue.Enqueue(operatorStack.Pop());

        // Build expression tree from postfix notation
        var stack = new Stack<Expr>();

        while (outputQueue.Count > 0)
        {
            var item = outputQueue.Dequeue();
            if (item is Expr expr)
            {
                stack.Push(expr);
            }
            else if (item is InfixOperator op)
            {
                if (stack.Count < 2)
                    throw new ParseException(TreeErrorType.EmptyExprList, 0);
                var right = stack.Pop();
                var left = stack.Pop();
                if (op.Symbol == ".")
                {
                    // Desugar a . b as a (b)
                    stack.Push(Expr.App(left, right));
                }
                else if (op.Symbol == "|>")
                {
                    // For chaining: a |> f |> g ==> g (f a)
                    stack.Push(Expr.App(right, left));
                }
                else
                {
                    // Convert infix operation to function application: op a b -> ((op a) b)
                    var functionVar = Expr.Var(op.GetFunctionName());
                    var result = Expr.App(Expr.App(functionVar, left), right);
                    stack.Push(result);
                }
            }
        }
        // For pipeline and composition operator, chaining produces nested Expr.Apps as desired
        if (stack.Count != 1)
            throw new ParseException(TreeErrorType.EmptyExprList, 0);
        return stack.Pop();
    }
    private Expr ParseParenthesizedExpr(List<Token> tokens, ref int i, int end)
    {
        int start = i, nesting = 0;
        for (var j = i + 1; j <= end; j++)
        {
            if (tokens[j].Type == TokenType.LParen) nesting++;
            else if (tokens[j].Type == TokenType.RParen)
            {
                if (nesting == 0)
                {
                    var expr = BuildExpressionTree(tokens, start + 1, j - 1);
                    i = j;
                    return expr;
                }
                nesting--;
            }
        }
        throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
    }
    private Expr ParseLetExpr(List<Token> tokens, ref int i, int end)
    {
        var letTokenPos = tokens[i].Position;
        i++; // Move past 'let'

        var isRecursive = false;
        if (i <= end && tokens[i].Type == TokenType.Rec)
        {
            isRecursive = true;
            i++; // Move past 'rec'
        }

        if (i > end)
            throw new ParseException(TreeErrorType.MissingLetVariable, letTokenPos);

        // Parse multiple assignments: let x = 1, y = 2, ... in ...
        var varNames = new List<string>();
        var valueExprs = new List<Expr>();
        while (i <= end) {
            // Parse variable name
            var varToken = tokens[i];
            if (varToken.Value is null)
                throw new ParseException(TreeErrorType.MissingLetVariable, varToken.Position);
            var varName = varToken.Value;
            varNames.Add(varName);
            i++;

            if (i > end || tokens[i].Type != TokenType.Equals)
                throw new ParseException(TreeErrorType.MissingLetEquals, tokens.ElementAtOrDefault(i)?.Position ?? letTokenPos);
            i++;

            // Debug all tokens in the range first
            
            // Find end of value expression (comma or 'in' at top level)
            // Need to handle arrow functions properly
            var valueStart = i;
            var valueEnd = -1;
            var nesting = 0;
            var letNesting = 0;
            var hasArrowInThisExpression = false;
            
            // First pass: check if this value expression contains an arrow function
            // Also track nested let expressions properly
            for (var k = i; k <= end; k++) {
                var token = tokens[k];
                if (token.Type is TokenType.LParen or TokenType.LBracket) nesting++;
                else if (token.Type is TokenType.RParen or TokenType.RBracket) nesting--;
                else if (nesting == 0 && token.Type == TokenType.Let) letNesting++;
                else if (nesting == 0 && token.Type == TokenType.In) {
                    if (letNesting > 0) letNesting--;
                    else break; // This 'in' belongs to our let - stop scanning
                }
                else if (nesting == 0 && letNesting == 0 && token.Type == TokenType.Arrow) {
                    hasArrowInThisExpression = true;
                    // Don't break here - we still need to find the proper end of our expression
                }
            }
            
            // Second pass: find boundary with arrow function awareness and proper let nesting
            nesting = 0;
            letNesting = 0;
            for (var j = i; j <= end; j++) {
                var token = tokens[j];

                if (token.Type is TokenType.LParen or TokenType.LBracket) nesting++;
                else if (token.Type is TokenType.RParen or TokenType.RBracket) nesting--;
                else if (nesting == 0 && token.Type == TokenType.Let) letNesting++;
                else if (nesting == 0 && token.Type == TokenType.In) {
                    if (letNesting > 0) letNesting--;
                    else {
                        // This 'in' belongs to our current let - stop here
                        valueEnd = j - 1;
                        break;
                    }
                }
                else if (nesting == 0 && letNesting == 0 && token.Type == TokenType.Comma && !hasArrowInThisExpression) {
                    // Only treat comma as boundary if this expression doesn't contain an arrow function
                    // and we're not inside a nested let
                    valueEnd = j - 1;
                    break;
                }
            }
            if (valueEnd == -1) // No comma or in found, must be last assignment
                valueEnd = end;
            if (valueStart > valueEnd)
                throw new ParseException(TreeErrorType.MissingLetValue, tokens.ElementAtOrDefault(i)?.Position ?? letTokenPos);
            
            // Debug the value expression tokens
            
            valueExprs.Add(BuildExpressionTree(tokens, valueStart, valueEnd));
            i = valueEnd + 1;

            // Accept comma, 'in', or next variable name (Term) as valid next tokens
            while (i <= end && tokens[i].Type == TokenType.Comma)
                i++;
            if (i <= end && tokens[i].Type == TokenType.In) {
                i++;
                break;
            }
            // If next token is Term, assume it's the next variable assignment (no comma required)
            if (i <= end && tokens[i].Type == TokenType.Term) continue;
            // If not at end, and not comma, in, or Term, error
            if (i <= end && tokens[i].Type != TokenType.Comma && tokens[i].Type != TokenType.In && tokens[i].Type != TokenType.Term)
                throw new ParseException(TreeErrorType.MissingLetIn, tokens.ElementAtOrDefault(i)?.Position ?? letTokenPos);
        }

        if (i > end)
            throw new ParseException(TreeErrorType.MissingLetBody, tokens.ElementAtOrDefault(i - 1)?.Position ?? letTokenPos);

        var bodyExpr = BuildExpressionTree(tokens, i, end);
        i = end; // Consume the rest of the tokens for the parent loop

        // Desugar let x = a, y = b in body as (\x y. body) a b
        // For let rec, only allow one variable for now (could be extended)
        // Desugar let rec x = a in body as (\x. Y (\x. body)) a
        if (isRecursive)
        {
            if (varNames.Count != 1)
                throw new ParseException(TreeErrorType.IllegalAssignment, letTokenPos); // Only one var for let rec
            var recVar = varNames[0];
            var recValue = valueExprs[0];
            var finalValueExpr = Expr.App(Expr.YCombinator(), Expr.Abs(recVar, recValue));
            return Expr.App(Expr.Abs(recVar, bodyExpr), finalValueExpr);
        }
        else
        {
            // Build nested lambdas for all variables
            var abs = varNames.AsEnumerable().Reverse().Aggregate(bodyExpr, (body, v) => Expr.Abs(v, body));
            // Apply all value expressions in order
            var app = abs;
            foreach (var val in valueExprs)
                app = Expr.App(app, val);
            return app;
        }
    }
    private Expr ParseLambdaExpr(List<Token> tokens, ref int i, int end)
    {
        var variables = new List<string>();
        int underscoreCount = 0;
        i++; // Skip λ or \

        // Collect all variables before the dot
        while (i <= end && tokens[i].Type == TokenType.Term)
        {
            var v = tokens[i].Value!;
            if (v == "_")
            {
                underscoreCount++;
                v = $"_placeholder{underscoreCount}";
            }
            variables.Add(v);
            i++;
        }

        if (variables.Count == 0)
            throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[i - 1].Position);

        // Check for dot separator
        if (i > end || tokens[i].Type != TokenType.Dot)
        {
            // Always require a dot after the parameter list for lambda abstractions
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i - 1].Position);
        }

        i++; // Skip dot

        if (i > end)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i - 1].Position);

        var lambdaBody = BuildExpressionTree(tokens, i, end);
        i = end;

        // Build nested lambdas from right to left (innermost to outermost)
        return variables.AsEnumerable().Reverse()
            .Aggregate(lambdaBody, (body, var) => Expr.Abs(var, body));
    }
    private Expr CreateChurchNumeral(int n) => n < 1
        ? Expr.Abs("f", Expr.Abs("x", Expr.Var("x")))
        : Expr.Abs("f", Expr.Abs("x", GenerateApplicationChain("f", "x", n)));
    private Expr GenerateApplicationChain(string funcVar, string baseVar, int count) =>
        Enumerable.Range(0, count).Aggregate(Expr.Var(baseVar), (acc, _) => Expr.App(Expr.Var(funcVar), acc));
    private Expr ParseListExpr(List<Token> tokens, ref int i, int end)
    {
        var start = i;
        var elements = new List<Expr>();
        
        i++; // Skip the opening bracket
        
        while (i <= end && tokens[i].Type != TokenType.RBracket)
        {
            var elementStart = i;
            // Check for range syntax: Integer .. Integer (now using TokenType.Range)
            if (tokens[i].Type == TokenType.Integer && i + 2 <= end && tokens[i + 1].Type == TokenType.Range && tokens[i + 2].Type == TokenType.Integer)
            {
                if (int.TryParse(tokens[i].Value, out int from) && int.TryParse(tokens[i + 2].Value, out int to))
                {
                    if (from <= to)
                    {
                        for (int v = from; v <= to; v++)
                            elements.Add(CreateChurchNumeral(v));
                    }
                    else
                    {
                        for (int v = from; v >= to; v--)
                            elements.Add(CreateChurchNumeral(v));
                    }
                    i += 3;
                    if (i <= end && tokens[i].Type == TokenType.Comma)
                        i++;
                    continue;
                }
            }
            // Find the end of this element (up to comma or closing bracket)
            var elementEnd = i;
            var nesting = 0;
            while (elementEnd <= end)
            {
                var token = tokens[elementEnd];
                if (token.Type is TokenType.LParen or TokenType.LBracket)
                    nesting++;
                else if (token.Type is TokenType.RParen or TokenType.RBracket)
                {
                    if (nesting == 0)
                        break;
                    nesting--;
                }
                else if (nesting == 0 && token.Type == TokenType.Comma)
                    break;
                elementEnd++;
            }
            if (elementStart < elementEnd)
            {
                var element = BuildExpressionTree(tokens, elementStart, elementEnd - 1);
                elements.Add(element);
            }
            i = elementEnd;
            if (i <= end && tokens[i].Type == TokenType.Comma)
                i++; // Skip comma
        }
        
        if (i > end || tokens[i].Type != TokenType.RBracket)
            throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
        
        // Build the list as nested cons expressions
        // [a, b, c] becomes cons a (cons b (cons c nil))
        var result = Expr.Var("nil");
        for (var j = elements.Count - 1; j >= 0; j--)
        {
            result = Expr.App(Expr.App(Expr.Var("cons"), elements[j]), result);
        }
        
        return result;
    }
    
    // Macro system methods
    private MacroDefinition ParseMacroDefinition(List<Token> tokens)
    {
        // Expected format: :macro (name pattern...) => transformation
        if (tokens.Count < 6 || tokens[0].Type != TokenType.Macro)
            throw new ParseException(TreeErrorType.IllegalAssignment, tokens[0].Position);
        
        if (tokens[1].Type != TokenType.LParen)
            throw new ParseException(TreeErrorType.UnclosedParen, tokens[1].Position);
        
        if (tokens[2].Type != TokenType.Term)
            throw new ParseException(TreeErrorType.MissingLetVariable, tokens[2].Position);
        
        var macroName = tokens[2].Value!;
        var patterns = new List<MacroPattern>();
        
        // Parse pattern
        var i = 3;
        while (i < tokens.Count && tokens[i].Type != TokenType.RParen)
        {
            patterns.Add(ParseMacroPattern(tokens, ref i));
        }
        
        if (i >= tokens.Count || tokens[i].Type != TokenType.RParen)
            throw new ParseException(TreeErrorType.UnclosedParen, tokens[2].Position);
        
        i++; // Skip ')'
        
        if (i >= tokens.Count || tokens[i].Type != TokenType.FatArrow)
            throw new ParseException(TreeErrorType.IllegalAssignment, tokens[i - 1].Position);
        
        i++; // Skip '=>'
        
        // Parse transformation
        var transformation = BuildExpressionTree(tokens, i, tokens.Count - 1);
        
        return new MacroDefinition(macroName, patterns, transformation);
    }
    
    private MacroPattern ParseMacroPattern(List<Token> tokens, ref int i)
    {
        var token = tokens[i];
        
        switch (token.Type)
        {
            case TokenType.Dollar:
                i++; // Skip '$'
                if (i >= tokens.Count || tokens[i].Type != TokenType.Term)
                    throw new ParseException(TreeErrorType.MissingLetVariable, token.Position);
                var varName = tokens[i].Value!;
                i++; // Skip variable name
                return MacroPattern.Variable(varName);
                
            case TokenType.LParen:
                var nestedPatterns = new List<MacroPattern>();
                i++; // Skip '('
                while (i < tokens.Count && tokens[i].Type != TokenType.RParen)
                {
                    var nestedPattern = ParseMacroPattern(tokens, ref i);
                    nestedPatterns.Add(nestedPattern);
                }
                if (i >= tokens.Count || tokens[i].Type != TokenType.RParen)
                    throw new ParseException(TreeErrorType.UnclosedParen, token.Position);
                i++; // Skip ')'
                return MacroPattern.List(nestedPatterns);
                
            case TokenType.Term:
                var value = token.Value!;
                i++; // Skip term
                return MacroPattern.Literal(value);
                
            default:
                throw new ParseException(TreeErrorType.IllegalAssignment, token.Position);
        }
    }
    
    public Expr ExpandMacros(Expr expr)
    {
        return ExpandMacrosRecursive(expr, new Dictionary<string, Expr>(), 0);
    }
    
    private Expr ExpandMacrosRecursive(Expr expr, Dictionary<string, Expr> bindings, int depth)
    {
        if (depth > 100) // Prevent infinite recursion
            return expr;
            
        // Try to match against macro patterns
        foreach (var (macroName, macro) in _macros)
        {
            var result = TryExpandMacro(expr, macro, depth);
            if (result.Success)
            {
                return result.ExpandedExpr!;
            }
        }
        
        // Recursively expand subexpressions
        return expr.Type switch
        {
            ExprType.Abs => Expr.Abs(expr.AbsVarName!, ExpandMacrosRecursive(expr.AbsBody!, bindings, depth)),
            ExprType.App => Expr.App(
                ExpandMacrosRecursive(expr.AppLeft!, bindings, depth),
                ExpandMacrosRecursive(expr.AppRight!, bindings, depth)),
            _ => expr
        };
    }
    
    private MacroExpansionResult TryExpandMacro(Expr expr, MacroDefinition macro, int depth)
    {
        var bindings = new Dictionary<string, Expr>();
        
        // Check if expression matches the macro pattern structure
        if (!MatchesMacroStructure(expr, macro))
        {
            return MacroExpansionResult.Failed("Structure mismatch");
        }
        
        // Try to match the pattern
        if (TryMatchPattern(expr, macro.Pattern, bindings))
        {
            var expandedTransformation = SubstituteMacroVariables(macro.Transformation, bindings);
            var result = ExpandMacrosRecursive(expandedTransformation, bindings, depth + 1);
            return MacroExpansionResult.Successful(result);
        }
        
        return MacroExpansionResult.Failed("Pattern match failed");
    }
    
    private bool MatchesMacroStructure(Expr expr, MacroDefinition macro)
    {
        // Check if the expression has the right structure to match this macro
        
        // The patterns list contains only argument patterns, not the macro name
        var expectedArgs = macro.Pattern.Count;
        
        if (expectedArgs == 0)
        {
            // Simple case: macro with no arguments, just check if expr is the macro name
            var result = expr.Type == ExprType.Var && expr.VarName == macro.Name;
            return result;
        }
        
        // For macros with arguments, we need to find the macro name at the base of the application chain
        var current = expr;
        var actualArgs = 0;
        
        // Count how many applications we have by walking down the left side
        while (current.Type == ExprType.App)
        {
            actualArgs++;
            current = current.AppLeft!;
        }
        
        // Check if we found the macro name at the base and have the right number of arguments
        var result2 = current.Type == ExprType.Var && current.VarName == macro.Name && actualArgs == expectedArgs;
        return result2;
    }
    
    private bool TryMatchPattern(Expr expr, IList<MacroPattern> patterns, Dictionary<string, Expr> bindings)
    {
        if (patterns.Count == 0)
            return true;
        
        // Since the macro name is checked separately in MatchesMacroStructure,
        // we only need to match the argument patterns here.
        // For (simple 42) matching macro (simple $x), we have:
        // - expr = App(Var("simple"), Number(42))
        // - patterns = [VariablePattern{Name="x"}]
        
        var current = expr;
        var args = new List<Expr>();
        
        // Extract arguments from right-to-left application chain
        for (int i = patterns.Count - 1; i >= 0; i--)
        {
            if (current.Type != ExprType.App)
                return false;
            args.Insert(0, current.AppRight!);
            current = current.AppLeft!;
        }
        
        // Match each argument against its pattern
        for (int i = 0; i < args.Count; i++)
        {
            if (!MatchSinglePattern(args[i], patterns[i], bindings))
                return false;
        }
        
        return true;
    }
    
    private bool MatchSinglePattern(Expr expr, MacroPattern pattern, Dictionary<string, Expr> bindings)
    {
        return pattern switch
        {
            LiteralPattern literal => expr.Type == ExprType.Var && expr.VarName == literal.Value,
            VariablePattern variable => (bindings[variable.Name] = expr) == expr, // Always succeeds, captures the expression
            ListPattern list => TryMatchPattern(expr, list.Patterns, bindings),
            _ => false
        };
    }
    
    private Expr SubstituteMacroVariables(Expr transformation, Dictionary<string, Expr> bindings)
    {
        return transformation.Type switch
        {
            ExprType.Var when transformation.VarName != null && transformation.VarName.StartsWith("__MACRO_VAR_") =>
                // Handle special macro variable placeholders
                ExtractAndSubstituteMacroVariable(transformation.VarName, bindings) ?? transformation,
            // Don't substitute regular variables - only macro variables should be substituted
            // This fixes the bug where literal numbers like "10" in macro definitions were being treated as variables
            ExprType.Abs => SubstituteInLambda(transformation, bindings),
            ExprType.App => Expr.App(
                SubstituteMacroVariables(transformation.AppLeft!, bindings),
                SubstituteMacroVariables(transformation.AppRight!, bindings)),
            _ => transformation
        };
    }
    
    private Expr SubstituteInLambda(Expr lambdaExpr, Dictionary<string, Expr> bindings)
    {
        // Process lambda parameter name - if it's a macro variable, substitute it
        var paramName = lambdaExpr.AbsVarName!;
        if (paramName.StartsWith("__MACRO_VAR_"))
        {
            var extractedVar = ExtractAndSubstituteMacroVariable(paramName, bindings);
            if (extractedVar != null && extractedVar.Type == ExprType.Var && extractedVar.VarName != null)
            {
                paramName = extractedVar.VarName;
            }
        }
        return Expr.Abs(paramName, SubstituteMacroVariables(lambdaExpr.AbsBody!, bindings));
    }
    
    private Expr? ExtractAndSubstituteMacroVariable(string macroVarName, Dictionary<string, Expr> bindings)
    {
        // Extract variable name from "__MACRO_VAR_variablename" format
        const string prefix = "__MACRO_VAR_";
        if (macroVarName.StartsWith(prefix))
        {
            var varName = macroVarName.Substring(prefix.Length);
            if (bindings.TryGetValue(varName, out var value))
            {
                return value;
            }
        }
        return null;
    }
    
    public string DefineMacro(string name, IList<MacroPattern> pattern, Expr transformation)
    {
        try
        {
            var macro = new MacroDefinition(name, pattern, transformation);
            _macros[name] = macro;
            return $"Macro '{name}' defined successfully";
        }
        catch (Exception ex)
        {
            return $"Error defining macro '{name}': {ex.Message}";
        }
    }
    
    public string ParseAndDefineMacro(string input)
    {
        try
        {
            var tokens = Tokenize(input);
            var macro = ParseMacroDefinitionFromInput(tokens);
            
            _macros[macro.Name] = macro;
            return $"Macro '{macro.Name}' defined successfully";
        }
        catch (Exception ex)
        {
            return $"Error defining macro: {ex.Message}";
        }
    }
    
    private MacroDefinition ParseMacroDefinitionFromInput(List<Token> tokens)
    {
        // Expected format: (name pattern...) => transformation
        if (tokens.Count < 5)
            throw new ParseException(TreeErrorType.IllegalAssignment, 0);
        
        if (tokens[0].Type != TokenType.LParen)
            throw new ParseException(TreeErrorType.UnclosedParen, tokens[0].Position);
        
        if (tokens[1].Type != TokenType.Term)
            throw new ParseException(TreeErrorType.MissingLetVariable, tokens[1].Position);
        
        var macroName = tokens[1].Value!;
        var patterns = new List<MacroPattern>();
        
        // Parse pattern
        var i = 2;
        while (i < tokens.Count && tokens[i].Type != TokenType.RParen)
        {
            var pattern = ParseMacroPattern(tokens, ref i);
            patterns.Add(pattern);
        }
        
        if (i >= tokens.Count || tokens[i].Type != TokenType.RParen)
            throw new ParseException(TreeErrorType.UnclosedParen, tokens[1].Position);
        
        i++; // Skip ')'
        
        if (i >= tokens.Count || tokens[i].Type != TokenType.FatArrow)
            throw new ParseException(TreeErrorType.IllegalAssignment, i < tokens.Count ? tokens[i].Position : tokens[tokens.Count - 1].Position);
        
        i++; // Skip '=>'
        
        // Parse transformation
        var transformation = ParseMacroTransformation(tokens, i, tokens.Count - 1);
        
        return new MacroDefinition(macroName, patterns, transformation);
    }
    
    private Expr ParseMacroTransformation(List<Token> tokens, int startIndex, int endIndex)
    {
        // Simple approach: convert tokens then parse
        // Special handling for lambda expressions with macro variables
        var processedTokens = new List<Token>();
        
        for (int i = startIndex; i <= endIndex; i++)
        {
            if (tokens[i].Type == TokenType.Lambda && i + 2 <= endIndex && 
                tokens[i + 1].Type == TokenType.Dollar && tokens[i + 2].Type == TokenType.Term)
            {
                // Handle λ$var pattern - convert to λ__MACRO_VAR_var and look for next dot
                var varName = tokens[i + 2].Value!;
                processedTokens.Add(tokens[i]); // Add λ
                processedTokens.Add(new Token(TokenType.Term, tokens[i + 2].Position, $"__MACRO_VAR_{varName}"));
                
                // Check if the next token is a dot that should be a lambda body separator
                if (i + 3 <= endIndex && tokens[i + 3].Type == TokenType.InfixOp && tokens[i + 3].Value == ".")
                {
                    processedTokens.Add(new Token(TokenType.Dot, tokens[i + 3].Position, "."));
                    i += 3; // Skip $, Term, and InfixOp dot
                }
                else
                {
                    i += 2; // Skip $ and Term
                }
            }
            else if (tokens[i].Type == TokenType.Dollar && i + 1 <= endIndex && tokens[i + 1].Type == TokenType.Term)
            {
                // Handle standalone $var
                var varName = tokens[i + 1].Value!;
                processedTokens.Add(new Token(TokenType.Term, tokens[i + 1].Position, $"__MACRO_VAR_{varName}"));
                i++; // Skip the Term token
            }
            else
            {
                processedTokens.Add(tokens[i]);
            }
        }
        
        return BuildExpressionTree(processedTokens, 0, processedTokens.Count - 1);
    }
    
    public string ListMacros()
    {
        if (_macros.Count == 0)
            return "No macros defined";
            
        var result = new System.Text.StringBuilder("Defined macros:\n");
        foreach (var (name, macro) in _macros)
        {
            result.AppendLine($"  {macro}");
        }
        return result.ToString().TrimEnd();
    }
}

public class Logger
{
    private string _logFile = "";
    private StreamWriter? _logWriter;
    private readonly SemaphoreSlim _logFileLock = new(1);

    private const string RED = "\u001b[31m";
    private const string GREEN = "\u001b[32m";
    private const string YELLOW = "\u001b[33m";
    private const string BLUE = "\u001b[34m";
    private const string MAGENTA = "\u001b[35m";
    private const string CYAN = "\u001b[36m";
    private const string WHITE = "\u001b[37m";
    private const string GRAY = "\u001b[90m";
    private const string RESET = "\u001b[0m";

    public static string Prompt(string txt) => $"{CYAN}{txt}{RESET} ";
    public string LogStatus => _logFile == "" ? "DISABLED" : _logFile;

    public async Task<string> HandleLogCommandAsync(string arg) => arg switch
    {
        "off" or "" => (_logFile = "", "Logging is disabled.").Item2,
        "clear" => await ClearLogFileAsync(),
        _ => (_logFile = arg, $"Logging is enabled to '{arg}'").Item2
    };

    public async Task<string> ClearLogFileAsync()
    {
        try
        {
            await CloseLogFileAsync();
            await File.WriteAllTextAsync(_logFile, "");
            return $"Log file '{_logFile}' cleared.";
        }
        catch (Exception ex)
        {
            return $"Error: clearing log file: {ex.Message}";
        }
    }

    public async Task CloseLogFileAsync()
    {
        if (_logWriter is null) return;
        await _logWriter.DisposeAsync();
        _logWriter = null;
    }

    private static string GetMessageColor(string message) => message switch
    {
        string s when s.StartsWith("Error:") => RED,
        string s when s.StartsWith("#") => YELLOW,        // Comments
        string s when s.StartsWith("->") => GREEN,        // Results/Assignments
        string s when s.StartsWith("Step") => YELLOW,     // Evaluation steps
        string s when s.StartsWith("Time:") => BLUE,      // Timing info
        string s when s.StartsWith("Name:") => BLUE,      // Final result details
        string s when s.StartsWith("Eval:") => MAGENTA,   // Evaluation expression
        string s when s.Contains("Loading") => CYAN,      // Loading files
        string s when s.Contains("<<") => GRAY,           // Reading file lines
        string s when s.Contains(">>") => GREEN,          // Result of reading file lines
        _ => RESET                                        // Default
    };

    public static void LogToConsole(string message) =>
        Console.WriteLine($"{GetMessageColor(message)}{message.Replace("\t", RESET)}{RESET}");

    public async Task LogAsync(string message, bool toConsole = true)
    {
        if (toConsole) LogToConsole(message);
        if (string.IsNullOrWhiteSpace(_logFile)) return;

        await _logFileLock.WaitAsync();
        try
        {
            _logWriter ??= new StreamWriter(_logFile, append: true, encoding: System.Text.Encoding.UTF8);
            await _logWriter.WriteLineAsync(message);
        }
        catch (Exception ex)
        {
            LogToConsole($"Error: writing to log file: {ex.Message}");
        }
        _logFileLock.Release();
    }

    public void Log(string message, bool toConsole = true) =>
        LogAsync(message, toConsole)
            .GetAwaiter()
            .GetResult(); // Synchronous version for compatibility with existing code
}

// Used for stack-based substitution in Interpreter
internal enum SubstOp { Evaluate, BuildAbs, BuildApp, SubstituteInBody }
internal readonly record struct StackEntry(Expr Node, SubstOp Op, object? Extra);

public class Interpreter
{
    private readonly Dictionary<string, Expr> _context = new(StringComparer.Ordinal);
    private readonly Dictionary<SubstitutionCacheKey, Expr> _substitutionCache = new(8192);
    private readonly Dictionary<Expr, Expr> _evaluationCache = new(8192, new ExprEqualityComparer());
    private readonly Dictionary<Expr, HashSet<string>> _freeVarCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _expressionPool = new(2048, StringComparer.Ordinal);
    private readonly Dictionary<(Expr, string), bool> _containsVarCache = new(2048);
    private readonly Dictionary<Expr, Expr> _normalizationCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _variableCache = new(1024);
    private readonly Logger _logger;
    private readonly InterpreterStats _stats;
    private readonly Parser _parser = new();
    private readonly System.Diagnostics.Stopwatch _perfStopwatch = new();
    private bool _showStep = false;
    private bool _lazyEvaluation = true;
    private bool _prettyPrint = true;
    private int _nativeArithmetic = 0;
    private bool _useNativeArithmetic = true;
    private bool _usedRandom = false;

    public Interpreter(Logger logger, InterpreterStats? stats = null)
    {
        _logger = logger;
        _stats = stats ?? new InterpreterStats();
    }

    public async Task<(Expr? exp, string str)> ProcessInputAsync(string input)
    {
        try
        {
            _stats.Iterations = 0;
            _usedRandom = false;
            if (string.IsNullOrWhiteSpace(input)) return (null, "");
            input = input.TrimEnd('\\');

            // Comments
            if (input.StartsWith('#')) return (null, input.Trim());

            // Handle commands
            if (input.Trim().StartsWith(':'))
                return (null, await HandleCommandAsync(input));

            var statement = _parser.Parse(input);
            if (statement == null) return (null, "");
            if (statement.Type == StatementType.Assignment)
            {
                var evaluatedExpression = EvaluateCEK(statement.Expression);
                _context[statement.VarName!] = evaluatedExpression;
                return (null, $"-> {statement.VarName} = {FormatWithNumerals(evaluatedExpression)}");
            }
            _logger.Log($"Eval: {FormatWithNumerals(statement.Expression)}");
            if (_showStep)
                _logger.Log($"Processing: {statement}");
            var result = EvaluateCEK(statement.Expression);
            var normalizedResult = NormalizeExpression(result);
            _stats.TotalIterations += _stats.Iterations;

            return (normalizedResult, $"-> {FormatWithNumerals(normalizedResult)}");
        }
        catch (Exception ex)
        {
            return (null, $"Error: {ex.Message}");
        }
        finally
        {
            _stats.VarCounter = 0; // Reset recursion depth counter
        }
    }

    private async Task DisplayOutput((Expr? expr, string str) result, TimeSpan elapsed)
    {   
        if (result.expr is not null)
        {
            var number = ExtractChurchNumeralValue(result.expr);
            var names = _context.Where(kv => kv.Value.StructuralEquals(result.expr))
                .Select(kv => kv.Key).ToList();

            var resultStr = 
                (number is not null ? $"[Church numeral: {number:#,##0}] " : "") +
                (names.Count > 0 ? $"[defined: {string.Join(", ", names)}]" : "");
            if (resultStr.Length > 2)
                await _logger.LogAsync($"Name: {resultStr}");

            var timeInfo = elapsed.TotalSeconds >= 1
                ? $"{elapsed.TotalSeconds:F2} s"
                : $"{elapsed.TotalMilliseconds:F1} ms";
            await _logger.LogAsync($"Time: {timeInfo}, iterations: {_stats.Iterations:#,##0}");
        }

        await _logger.LogAsync(result.str);
        await _logger.LogAsync($" ");
    }

    // Common input processing for both REPL and file loading
    private async Task ProcessAndDisplayInputAsync(string input)
    {
        var timing = System.Diagnostics.Stopwatch.StartNew();
        var output = await ProcessInputAsync(input);
        timing.Stop();
        await DisplayOutput(output, timing.Elapsed);
    }

    private Expr EvaluateCEK(Expr expr, Dictionary<string, Expr>? initialEnv = null)
    {
        _perfStopwatch.Restart();
        var environment = new Dictionary<string, Expr>(initialEnv ?? _context, StringComparer.Ordinal);
        var stateStack = new Stack<CEKState>();
        stateStack.Push(new CEKState(expr, environment, Kontinuation.Empty));
        int currentStep = 0;
        Expr? finalResult = null;
        const int maxIterations = 200000; // Prevent infinite loops

        while (stateStack.Count > 0 && _stats.Iterations < maxIterations)
        {
            _stats.Iterations++;
            var (control, env, kont) = stateStack.Pop();
            if (_showStep)
                _logger.Log($"Step {currentStep++}: CEK \tC: {FormatWithNumerals(control)}, K: {kont.Type}");
            if (GetEvalCache(control, out var cachedResult))
            {
                ApplyContinuation(cachedResult, env, kont, stateStack, ref finalResult);
                continue;
            }

            // Native arithmetic shortcut for Church numerals
            if (control.Type == ExprType.App)
            {
                var nativeResult = TryNativeArithmetic(control, env);
                if (nativeResult != null)
                {
                    ApplyContinuation(nativeResult, env, kont, stateStack, ref finalResult);
                    if (finalResult != null)
                        break;
                    continue;
                }
            }

            switch (control)
            {
                case { Type: ExprType.Var, VarName: var v }:
                    var value = env.TryGetValue(v!, out var envValue) ? envValue : control;
                    ApplyContinuation(value, env, kont, stateStack, ref finalResult);
                    break;
                case { Type: ExprType.Abs }:
                    ApplyContinuation(control, env, kont, stateStack, ref finalResult);
                    break;
                case { Type: ExprType.App, AppLeft: var left, AppRight: var right }:
                    // Special handling for Church conditionals to ensure lazy evaluation of branches
                    if (IsChurchConditional(control, env))
                        HandleChurchConditional(control, env, kont, stateStack);
                    else
                    {
                        var argKont = Kontinuation.Arg(right!, env, kont);
                        stateStack.Push(new CEKState(left!, env, argKont));
                    }
                    break;
                case { Type: ExprType.Thunk }:
                    var forcedValue = Force(control);
                    ApplyContinuation(forcedValue, env, kont, stateStack, ref finalResult);
                    break;
                case { Type: ExprType.YCombinator }:
                    // The Y combinator is a value that can be applied to arguments
                    ApplyContinuation(control, env, kont, stateStack, ref finalResult);
                    break;
            }
            if (finalResult != null)
                break;
        }

        // Check if we hit the iteration limit
        if (_stats.Iterations >= maxIterations)
        {
            throw new InvalidOperationException($"Evaluation exceeded maximum iterations ({maxIterations}). This may indicate an infinite loop, possibly due to undefined variables or non-terminating recursion.");
        }

        _stats.TimeInEvaluation += _perfStopwatch.ElapsedTicks;
        if (finalResult != null)
        {
            // do not cache results if random numbers were used
            if (!_usedRandom)
                PutEvalCache(currentStep, expr, finalResult);
            return finalResult;
        }
        throw new InvalidOperationException("CEK evaluation completed without returning a value");
    }

    // Native arithmetic for Church numerals
    private Expr? TryNativeArithmetic(Expr app, Dictionary<string, Expr> env)
    {
        if (!_useNativeArithmetic)
            return null;

        // Unroll left-associative applications: (((op a) b) c) ...
        var args = new List<Expr>();
        Expr? cur = app;
        while (cur is { Type: ExprType.App, AppLeft: var l, AppRight: var r })
        {
            args.Insert(0, r!);
            cur = l;
        }
        if (cur is not { Type: ExprType.Var, VarName: var opName })
            return null;

        if (args.Count < 1 || args.Count > 2)
            return null; // Only support unary or binary operations

        if (!TryGetChurchInt(args[0], env, out var a))
            return null; // First argument must be a Church numeral
        var b = 0; // Default value for second argument if avaiable
        var isArg2Number = args.Count == 2 && TryGetChurchInt(args[1], env, out b);

        // Only intercept known arithmetic primitives
        int? result = (opName, args.Count, isArg2Number) switch
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
            ("double", 1, _) => a * 2,
            ("half", 1, _) => a / 2,
            ("sqrt", 1, _) => (int)Math.Sqrt(a),
            ("random", 1, _) => (_usedRandom = true, new Random().Next(0, a + 1)).Item2, // Random number in range [0, a]

            ("iszero", 1, _) => a == 0 ? -1 : -2,
            ("even", 1, _) => a % 2 == 0 ? -1 : -2,
            ("odd", 1, _) => a % 2 != 0 ? -1 : -2,

            ("lt" or "<", 2, true) => a < b ? -1 : -2,
            ("leq" or "<=", 2, true) => a <= b ? -1 : -2,
            ("eq" or "==", 2, true) => a == b ? -1 : -2,
            ("geq" or ">=", 2, true) => a >= b ? -1 : -2,
            ("gt" or ">", 2, true) => a > b ? -1 : -2,
            ("neq" or "!=", 2, true) => a != b ? -1 : -2,

            _ => null
        };

        if (result is null)
            return null; // Not a recognized operation or invalid arguments

        _nativeArithmetic++;
        // Negative results are used for boolean-like values
        return result.Value switch
        {
            -1 => Expr.Abs("f", Expr.Abs("x", Expr.Var("f"))), // Church true
            -2 => Expr.Abs("f", Expr.Abs("x", Expr.Var("x"))), // Church false
            _ => MakeChurchNumeral(result.Value) // Return Church numeral for non-negative results
        };
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

    private Expr Intern(Expr expr)
    {
        if (expr.Type != ExprType.Var) return expr;
        var key = $"var:{expr.VarName}";
        if (!_expressionPool.TryGetValue(key, out var existing))
            _expressionPool[key] = existing = expr;
        return existing;
    }

    private bool GetEvalCache(Expr expr, out Expr result)
        => GetFromCache(_evaluationCache, expr, out result);

    // Consolidated cache access pattern
    private bool GetFromCache<TKey, TValue>(Dictionary<TKey, TValue> cache, TKey key, out TValue result) 
        where TKey : notnull
        where TValue : class
    {
        if (key is not null && cache.TryGetValue(key, out var cachedResult) && cachedResult is not null)
        {
            _stats.CacheHits++;
            result = cachedResult;
            return true;
        }
        _stats.CacheMisses++;
        result = null!;
        return false;
    }

    private async Task InputLoopAsync(Func<string?, Task> handleInput, string promptPrimary = "lambda> ", string promptCont = "......> ")
    {
        var currentInput = new System.Text.StringBuilder();
        while (true)
        {
            if (currentInput.Length == 0)
                Console.WriteLine();
            Console.Write(Logger.Prompt(currentInput.Length == 0 ? promptPrimary : promptCont));
            var line = Console.ReadLine();
            if (line is null)
            {
                Console.WriteLine("\nGoodbye!");
                break;
            }
            if (string.IsNullOrWhiteSpace(line)) continue;
            await _logger.LogAsync($"λ> {line}", false);
            
            // Use consolidated line processing
            if (line.EndsWith('\\'))
            {
                currentInput.Append(line[..^1]);
                continue;
            }
            currentInput.Append(line);
            var input = currentInput.ToString();
            currentInput.Clear();
            await handleInput(input);
            if (input.Trim() == ":exit" || input.Trim() == ":quit")
                break;
        }
    }
    public async Task RunInteractiveLoopAsync()
        => await InputLoopAsync(async input => await ProcessAndDisplayInputAsync(input!));
    // Helper method to safely load a file if it exists
    public async Task<string> LoadFileIfExistsAsync(string path)
    {
        if (!File.Exists(path))
            return $"File not found: {path}";
        
        try
        {
            return await LoadFileAsync(path);
        }
        catch (Exception ex)
        {
            return $"Error loading {path}: {ex.Message}";
        }
    }

    // Consolidated line processing logic shared between interactive and file loading
    private async Task ProcessLineWithContinuation(string line, System.Text.StringBuilder currentInput, Func<string, Task> processInput, bool isFromFile = false, int lineNumber = 0)
    {
        if (isFromFile)
            await _logger.LogAsync($"line {lineNumber} <<: {line}");
        
        var trimmed = isFromFile ? line.TrimEnd() : line;
        
        if (isFromFile && (string.IsNullOrWhiteSpace(trimmed) || trimmed.StartsWith('#')))
            return;
        
        if (trimmed.EndsWith('\\'))
        {
            currentInput.Append(trimmed[..^1]);
            return;
        }
        
        currentInput.Append(trimmed);
        var input = currentInput.ToString();
        currentInput.Clear();
        await processInput(input);
    }

    public async Task<string> LoadFileAsync(string path)
    {
        int lineCount = 0;
        _logger.Log($"Loading commands from '{path}'");
        var lines = await File.ReadAllLinesAsync(path);
        var currentInput = new System.Text.StringBuilder();
        
        foreach (var line in lines)
        {
            await ProcessLineWithContinuation(line, currentInput, ProcessAndDisplayInputAsync, true, lineCount++);
        }
        
        if (currentInput.Length > 0)
            await ProcessAndDisplayInputAsync(currentInput.ToString());
        return $"Loaded {path}";
    }

    private async Task<string> HandleCommandAsync(string input)
    {
        var parts = input.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
        var command = parts[0];
        var arg = parts.Length > 1 ? parts[1].Trim() : "";
        return command switch
        {
            ":log" => await _logger.HandleLogCommandAsync(arg),
            ":load" => await LoadFileAsync(arg),
            ":save" => await SaveFileAsync(arg),
            ":step" => HandleStep(arg),
            ":lazy" => HandleLazy(arg),
            ":clear" => ClearEnvironment(),
            ":stats" => ShowStats(),
            ":help" => ShowHelp(),
            ":env" => ShowEnv(),
            ":memo" => MemoClear(),
            ":exit" or ":quit" => "bye",
            ":depth" => HandleRecursionDepth(arg),
            ":infix" => HandleInfixCommand(arg),
            ":native" => HandleNativeArithmetic(arg),
            ":pretty" => HandlePrettyPrint(arg),
            ":macros" => _parser.ListMacros(),
            ":macro" => HandleMacroDefinition(arg),
            _ => $"Unknown command: {command}"
        };
    }

    // Display all supported native arithmetic functions/operators
    private string ShowNativeFunctions()
    {
        return $"""
                Supported native arithmetic functions/operators (for Church numerals):

                Binary (two arguments):
                    plus, +         : addition
                    minus, -        : subtraction (clamped to 0)
                    mult, *         : multiplication
                    div, /          : integer division (0 if divisor is 0)
                    mod, %          : modulo (0 if divisor is 0)
                    exp, pow, ^     : exponentiation
                    max             : maximum
                    min             : minimum
                    lt, <           : less than (returns true/false)
                    leq, <=         : less than or equal (returns true/false)
                    eq, ==          : equal (returns true/false)
                    geq, >=         : greater than or equal (returns true/false)
                    gt, >           : greater than (returns true/false)
                    neq, !=         : not equal (returns true/false)

                Unary (one argument):
                    succ, ++        : successor (n+1)
                    pred, --        : predecessor (clamped to 0)
                    square          : n*n
                    double          : n*2
                    half            : n/2
                    sqrt            : integer square root
                    random          : random integer in [0, n]
                    iszero          : returns true if n==0, else false
                    even            : returns true if n is even
                    odd             : returns true if n is odd

                Notes:
                    - Boolean results are Church booleans (true = λf.λx.f, false = λf.λx.x)
                    - All arguments must be Church numerals (integers)
                    - These functions are only available when native arithmetic is enabled (:native on)
                """;
    }

    private string HandleNativeArithmetic(string arg)
    {
        if (arg == "show")
            return ShowNativeFunctions();

        _useNativeArithmetic = arg == "on";
        return "Native arithmetic " + (_useNativeArithmetic ? "enabled" : "disabled");
    }

    private string HandlePrettyPrint(string arg)
    {
        _prettyPrint = arg != "off";
        return $"Pretty printing {(_prettyPrint ? "enabled" : "disabled")}";
    }

    private string HandleStep(string arg)
    {
        _showStep = arg == "on";
        return $"Step mode {(_showStep ? "enabled" : "disabled")}";
    }

    private string HandleLazy(string arg)
    {
        _lazyEvaluation = arg != "off";
        return $"Lazy evaluation {(_lazyEvaluation ? "enabled" : "disabled")}";
    }

    private string HandleRecursionDepth(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg))
            return $"Current recursion depth limit: {_stats.MaxRecursionDepth}";

        if (int.TryParse(arg, out int value) && value >= 10 && value <= 10000)
        {
            _stats.MaxRecursionDepth = value;
            return $"Recursion depth limit set to {_stats.MaxRecursionDepth}";
        }

        return "Error: Please provide a number between 10 and 10000.";
    }

    private string HandleInfixCommand(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg))
            return ShowInfixOperators();

        var parts = arg.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        
        if (parts.Length != 3)
            return "Usage: :infix <operator> <precedence> <associativity>\nExample: :infix + 6 left";
        
        var symbol = parts[0];
        
        if (!int.TryParse(parts[1], out int precedence))
            return "Error: Precedence must be a number between 1 and 10";
        
        var associativity = parts[2].ToLowerInvariant();
        
        return _parser.DefineInfixOperator(symbol, precedence, associativity);
    }

    private string HandleMacroDefinition(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg))
            return "Usage: :macro (pattern) => transformation\nExample: :macro (when $cond $body) => (if $cond $body unit)";

        try
        {
            // Parse the macro definition from the argument string
            return _parser.ParseAndDefineMacro(arg);
        }
        catch (Exception ex)
        {
            return $"Error defining macro: {ex.Message}";
        }
    }

    // Consolidated formatting method that uses the enhanced Expr.ToString()
    private string FormatWithNumerals(Expr expr) => 
        expr.ToString(_prettyPrint, _prettyPrint ? ExtractChurchNumeralValue : null);

    // Force evaluation of a thunk (lazy value)
    private Expr Force(Expr expr)
    {
        if (expr.Type != ExprType.Thunk || expr.ThunkValue is null)
            return expr;

        if (expr.ThunkValue.IsForced)
            return expr.ThunkValue.ForcedValue!;

        if (expr.ThunkValue.IsBeingForced)
            return expr; // Return the thunk itself to handle recursion.

        if (_showStep)
            _logger.Log($"Step: Forcing thunk: \t{FormatWithNumerals(expr.ThunkValue.Expression)}");

        _perfStopwatch.Restart();
        _stats.ThunkForceCount++;

        expr.ThunkValue.BeginForce();

        // Evaluate the thunk's expression in its captured environment
        var forced = EvaluateCEK(expr.ThunkValue.Expression, expr.ThunkValue.Environment);

        _stats.TimeInForcing += _perfStopwatch.ElapsedTicks;

        // In-place update of the thunk
        expr.ThunkValue.Force(forced);
        return forced;
    }

    private string MemoClear()
    {
        _substitutionCache.Clear();
        _evaluationCache.Clear();
        _freeVarCache.Clear();
        _expressionPool.Clear();
        _stats.CacheHits = _stats.CacheMisses = 0;
        GC.Collect(); // Suggest garbage collection to free memory
        return "All caches cleared.";
    }

    private async Task<string> SaveFileAsync(string path)
    {
        await File.WriteAllLinesAsync(path, _context.Select(kv => $"{kv.Key} = {kv.Value}"));
        return $"Saved to {path}";
    }

    private string ClearEnvironment()
    {
        _context.Clear();
        MemoClear(); // Reuse cache clearing logic
        _stats.Reset();
        return "Environment cleared.";
    }

    private string ShowEnv()
    {
        _logger.Log("# Current environment:");
        foreach (var (key, value) in _context.OrderBy(kv => kv.Key))
            _logger.Log($"  {key} = {FormatWithNumerals(value)}");

        var infixDefs = ShowInfixOperators();
        _logger.Log(infixDefs);
        return $"# Displayed {_context.Count} definitions.";
    }

    public string ShowInfixOperators()
    {
        if (_parser._infixOperators.Count == 0)
            return "No infix operators defined";
        
        var operators = _parser._infixOperators.Values
            .OrderByDescending(op => op.Precedence)
            .ThenBy(op => op.Symbol);
        
        _logger.Log("Defined infix operators:\n");
        foreach (var op in operators)
            _logger.Log($"  infix {op.Symbol} (precedence: {op.Precedence}, associativity: {op.Associativity.ToString().ToLower()})");
        return $"# Displayed {_parser._infixOperators.Count} infix operators.";
    }

    private string ShowStats()
    {
        static string PerOfTotal(long value, long total) => total == 0 ? "0.0%" : $"{value * 100.0 / total:F1}%";
        var totalTime = _stats.TimeInCacheLookup + _stats.TimeInSubstitution + _stats.TimeInEvaluation + _stats.TimeInForcing;
        var process = System.Diagnostics.Process.GetCurrentProcess();
        var memUsage = GC.GetTotalMemory(false) / 1024.0;
        var peakMem = process.PeakWorkingSet64 / 1024.0;
        var threads = process.Threads.Count;
        var startTime = process.StartTime;
        var upTime = DateTime.Now - startTime;
        var gen0 = GC.CollectionCount(0);
        var gen1 = GC.CollectionCount(1);
        var gen2 = GC.CollectionCount(2);
        var evalMode = _lazyEvaluation ? "Lazy" : "Eager";
        var cacheHitRate = PerOfTotal(_stats.CacheHits, _stats.CacheHits + _stats.CacheMisses);
        var cacheStats = $"Subst: {_substitutionCache.Count}, Eval: {_evaluationCache.Count}, FreeVar: {_freeVarCache.Count}, Var: {_expressionPool.Count}, ContainsVar: {_containsVarCache.Count}, Norm: {_normalizationCache.Count}";
        return $"""
        === Lambda Interpreter Statistics ===
        
        -- Environment --
        Definitions:              {_context.Count:#,##0}, infix operators: {_parser._infixOperators.Count:#,##0}, native arithmetic: {(_useNativeArithmetic ? "ENABLED" : "DISABLED")}
        Unique expressions:       {_expressionPool.Count:#,##0}
        Unique var counter:       {_stats.VarCounter:#,##0}
        Pretty printing:          {(_prettyPrint ? "ENABLED" : "DISABLED")}
        
        -- Evaluation --
        Mode:                     {evalMode}
        Recursion depth limit:    {_stats.MaxRecursionDepth:#,##0}, max iterations: {200_000:#,##0}
        Step-by-step:             {(_showStep ? "ENABLED" : "DISABLED")}
        Normalizations:           {_stats.NormalizeCEKCount:#,##0}
        Thunks forced:            {_stats.ThunkForceCount:#,##0}
        Total iterations:         {_stats.TotalIterations:#,##0}
        Hash code calls:          {Expr.HashCodeCount:#,##0}
        Native arithmetic:        {(_useNativeArithmetic ? "ENABLED" : "DISABLED")}, {_nativeArithmetic:#,##0} calls

        -- Memoization/Caching --
        Cache sizes:              {cacheStats}
        Cache hits/misses:        {_stats.CacheHits:#,##0} / {_stats.CacheMisses:#,##0} ({cacheHitRate})
        
        -- Performance (timings) --
        Cache lookup time:        {_stats.TimeInCacheLookup / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInCacheLookup, totalTime)})
        Substitution time:        {_stats.TimeInSubstitution / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInSubstitution, totalTime)}) (calls: {_stats.SubstitutionExprCount:#,##0})
        Evaluation time:          {_stats.TimeInEvaluation / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInEvaluation, totalTime)})
        Thunk forcing time:       {_stats.TimeInForcing / 10000.0:#,##0.00} ms ({PerOfTotal(_stats.TimeInForcing, totalTime)})
        Total measured time:      {totalTime / 10000.0:#,##0.00} ms
        
        -- System --
        Logging:                  {_logger.LogStatus}
        Memory usage:             {memUsage:#,##0.0} KB (peak: {peakMem:#,##0.0} KB)
        Threads:                  {threads}
        Uptime:                   {upTime:dd\.hh\:mm\:ss}
        GC collections:           Gen0={gen0} Gen1={gen1} Gen2={gen2}
        Process start:            {startTime}
        """;
    }

    private static string ShowHelp() =>

        """
        ================= Lambda Calculus Interpreter Help =================

        -- Expression Syntax --
          x                      Variable (e.g., myVar)
          \x.expr or λx.expr     Lambda abstraction (e.g., \x.x or λf.λx.f x)
          \x y z.expr            Multi-argument lambda (sugar for \x.\y.\z.expr)
          x -> expr              Arrow function (sugar for \x.expr, e.g., x -> x + 1)
          x, y -> expr           Multi-parameter arrow function (sugar for \x.\y.expr)
          (expr)                 Grouping (e.g., (\x.x) y)
          expr1 expr2            Application (e.g., succ 0)
          let x = expr1 in expr2 Local binding (e.g., let id = \x.x in id 0) sugar for (\x.expr2) expr1
          let x = a, y = b in B  Multiple assignments in let (e.g., let x = 1, y = 2 in x + y) sugar for (\x.\y.B) a b
          let f = x -> x+1 in e1 Arrow functions in let (e.g., let add = x, y -> x + y in add 3 4)
          let rec f = E in B     Recursive local binding desugar to (\f.B) (Y (\f.E))
          name = expr            Assignment (e.g., id = \x.x)
          123                    Integer literal (Church numeral λf.λx.f^n(x))
          [a, b, c]              List literal (cons a (cons b (cons c nil)))
          [a .. b]               List range (syntactic sugar for [a, a+1, ..., b]) both asc and desc
          Y f1                   Y combinator (e.g., Y \f.\x.f (f x)) Y = λf.(λx.f (x x)) (λx.f (x x))
          a + b                  Infix operations (when operators are defined) desugar to plus a b
          a . b . c              composition operator desugar to a (b c)
          a |> f |> g            Pipeline operator desugar to g (f a)
          \_ . expr              Use '_' as a placeholder/ignored parameter in lambdas
          (x, _, _ -> x) 42 9 8  Multiple '_'s are allowed; each is treated as a unique, ignorable variable

        -- Commands (prefix with ':') --
          :clear                 Clear the current environment and caches
          :depth [n]             Set/show max recursion depth (default: 100, range: 10-10000)
          :env                   Show current environment definitions
          :help                  Show this help message
          :infix [op prec assoc] Define/show infix operators (e.g., :infix + 6 left)
          :lazy on|off           Toggle lazy evaluation (default: on) or (eager evaluation)
          :load <file>           Load definitions from file (e.g., :load stdlib.lambda)
          :log <file|off>        Log output to file or disable logging (e.g., :log session.log, :log off)
          :log clear             Clear the current log file (if enabled)
          :macro (pattern) => transformation  Define a macro (e.g., :macro (when $cond $body) => (if $cond $body unit))
          :macros                List all defined macros
          :memo                  Clear all memoization/caches
          :native on|off         Enable/disable native arithmetic for Church numerals (default: on)
          :native show           Show all supported native arithmetic functions/operators
          :pretty on|off         Toggle pretty printing (default: on) - numerals and lists
          :save <file>           Save current environment to file (e.g., :save myenv.lambda)
          :stats                 Show detailed performance and environment statistics
          :step on|off           Toggle step-by-step evaluation logging
          :exit, :quit           Exit the interpreter

        -- Macro System --
          :macro (name $var1 $var2) => transformation
                                 Define a macro with pattern matching
          Examples:
            :macro (when $cond $body) => (if $cond $body unit)
            :macro (unless $cond $body) => (if $cond unit $body)
            :macro (compose $f $g) => (\x. $f ($g x))
            :macro (flip $f) => (\x y. $f y x)

        -- Interactive Features --
          - Line continuation: Use '\' at end of line to continue input
          - Comments: Lines starting with '#' are ignored, or any text after '#' in a line is ignored
          - Command line arguments: Treated as files to load at startup
          - Infix operators: Define custom operators with precedence (1-10) and associativity (left/right)
          - Macro variables: Use $variable in patterns to capture expressions
          - Internally, each '_' is renamed to a unique variable (_placeholder1, _placeholder2, ...)
        """;

    private void PutEvalCache(int step, Expr expr, Expr result) => _evaluationCache.TryAdd(expr, result);

    private void ApplyContinuation(Expr value, Dictionary<string, Expr> env, Kontinuation kont, Stack<CEKState> stateStack, ref Expr? finalResult)
    {
        switch (kont)
        {
            case { Type: KontinuationType.Empty }:
                // Final result - no more continuation
                finalResult = value;
                return;

            case { Type: KontinuationType.Arg, Expression: var expr, Environment: var kenv, Next: var next }:
                // We have evaluated the function, now evaluate the argument
                var funKont = Kontinuation.Fun(value, next!);

                // In lazy evaluation, wrap the argument in a thunk instead of evaluating it immediately
                if (_lazyEvaluation)
                {
                    var thunk = Expr.Thunk(expr!, kenv!);
                    stateStack.Push(new CEKState(thunk, env, funKont));
                }
                else
                    stateStack.Push(new CEKState(expr!, kenv!, funKont));
                break;

            case { Type: KontinuationType.Fun, Value: var function, Next: var next }:
                // We have evaluated the argument, now apply the function (stored in kont.Value)
                var argument = value;

                var funcToApply = function;
                if (funcToApply!.Type == ExprType.Thunk)
                    funcToApply = Force(funcToApply);

                if (funcToApply.Type == ExprType.Abs)
                {
                    // Beta reduction - substitute argument for parameter in function body
                    var substituted = Substitute(funcToApply.AbsBody!, funcToApply.AbsVarName!, argument);
                    stateStack.Push(new CEKState(substituted, env, next!));
                }
                else if (funcToApply.Type == ExprType.YCombinator)
                {
                    // Y f becomes a thunk for f (Y f) to delay recursion.
                    var yf = Expr.App(Expr.YCombinator(), argument);
                    var recursiveExpr = Expr.App(argument, yf);
                    var thunkExpr = Expr.Thunk(recursiveExpr, env);
                    ApplyContinuation(thunkExpr, env, next!, stateStack, ref finalResult);
                }
                else
                {
                    // Not a function - create application and continue with next continuation
                    // This prevents infinite loops with undefined variables
                    var app = Expr.App(funcToApply, argument);
                    ApplyContinuation(app, env, next!, stateStack, ref finalResult);
                }
                break;

            case { Type: KontinuationType.Conditional, ThenBranch: var thenBranch, ElseBranch: var elseBranch, Environment: var condEnv, Next: var next }:
                // We have evaluated the condition, now choose the appropriate branch
                // The condition should be a Church boolean: true = λx.λy.x, false = λx.λy.y
                var selectedBranch = IsChurchTrue(value) ? thenBranch! : elseBranch!;
                stateStack.Push(new CEKState(selectedBranch, condEnv!, next!));
                break;
        }
    }

    // Alpha conversion to avoid variable capture
    private (string, Expr) AlphaConvert(string varName, Expr body)
    {
        var newVar = $"{varName}_{_stats.VarCounter++}";
        var newBody = Substitute(body, varName, Expr.Var(newVar));
        return (newVar, newBody);
    }

    // Expression interning for memory efficiency
    private HashSet<string> FreeVars(Expr expr, string? skipVar = null)
    {
        if (expr is null) return [];

        if (_freeVarCache.TryGetValue(expr, out var cached))
            return skipVar is null ? cached : [.. cached.Where(v => v != skipVar)];

        var freeVars = new HashSet<string>();
        var boundVars = new HashSet<string>();
        var stack = new Stack<(Expr Expr, bool InScope, string? BoundVar)>();

        stack.Push((expr, true, null));

        while (stack.Count > 0)
        {
            var (current, inScope, boundVar) = stack.Pop();

            if (!inScope && boundVar is not null)
            {
                boundVars.Remove(boundVar);
                continue;
            }

            switch (current.Type)
            {
                case ExprType.Var:
                    if (!boundVars.Contains(current.VarName!) && (skipVar is null || current.VarName != skipVar))
                        freeVars.Add(current.VarName!);
                    break;

                case ExprType.Abs when current.AbsVarName is not null && current.AbsBody is not null:
                    var absVar = current.AbsVarName;
                    boundVars.Add(absVar);
                    stack.Push((current, false, absVar)); // Cleanup entry
                    stack.Push((current.AbsBody, true, null));
                    break;

                case ExprType.App when current.AppLeft is not null && current.AppRight is not null:
                    stack.Push((current.AppRight, true, null));
                    stack.Push((current.AppLeft, true, null));
                    break;

                case ExprType.Thunk when current.ThunkValue is not null:
                    // For thunks, analyze the expression inside
                    stack.Push((current.ThunkValue.Expression, true, null));
                    break;

                case ExprType.YCombinator:
                    // Y combinator has no free variables
                    break;
            }
        }
        if (skipVar is null)
            _freeVarCache[expr] = freeVars;

        return freeVars;
    }

    private Expr? GetSubCache(Expr root, string var, Expr val)
    {
        var cacheKey = new SubstitutionCacheKey(root, var, val);
        return GetFromCache(_substitutionCache, cacheKey, out var result) ? result : null;
    }

    private void PutSubCache(Expr root, string var, Expr val, Expr result)
        => _substitutionCache[new SubstitutionCacheKey(root, var, val)] = result;

    private Expr Substitute(Expr root, string var, Expr val)
    {
        // Fast-path: If substituting a variable with itself, return the original expression
        if (_showStep)
            _logger.Log($"Step: Substitute \t{var} := {FormatWithNumerals(val)} in {FormatWithNumerals(root)}");

        if (val.Type == ExprType.Var && val.VarName == var)
            return root;

        // Fast-path: If the variable does not occur free in the expression, return the original
        if (!ContainsVariable(root, var))
            return root;

        // Always use substitution cache for all substitutions (move to top)
        Expr? cachedResult = GetSubCache(root, var, val);
        if (cachedResult is not null) return cachedResult;

        _stats.SubstitutionExprCount++;

        // Ultra-fast path for variables with aggressive caching
        if (root.Type == ExprType.Var)
        {
            if (_showStep && root.VarName == var)
                _logger.Log($"Step: Substitute variable \t{var} with {FormatWithNumerals(val)}");
            if (root.VarName == var) return val;

            if (_expressionPool.TryGetValue(root.VarName!, out var cached)) return cached;

            // Cache all variable lookups for better performance
            var toCache = Intern(root);
            _expressionPool[root.VarName!] = toCache;
            return toCache;
        }

        // Fast path for abstractions that shadow the variable
        if (root.Type == ExprType.Abs && root.AbsVarName == var) return root;
        
        // Handle thunks
        if (root.Type == ExprType.Thunk && root.ThunkValue is not null)
        {
            if (_showStep)
                _logger.Log($"Step: Substitute in thunk: \t{FormatWithNumerals(root.ThunkValue.Expression)}");
            var substitutedExpr = Substitute(root.ThunkValue.Expression, var, val);
            var newThunk = new Thunk(substitutedExpr, root.ThunkValue.Environment);
            if (root.ThunkValue.IsForced)
                newThunk.Force(root.ThunkValue.ForcedValue!);
            return root with { ThunkValue = newThunk };
        }

        // Ultra-fast application patterns for common Church numeral operations
        if (root.Type == ExprType.App && IsCommonPattern(root, var, val, out var fastResult))
            return fastResult;

        _perfStopwatch.Restart();

        Expr result;
        if (++_stats.VarCounter > _stats.MaxRecursionDepth)
            result = SubstituteStackBased(root, var, val);
        else
        {
            result = root.Type switch
            {
                ExprType.Abs when root.AbsVarName != var && QuickFreeVarCheck(val, root.AbsVarName!) =>
                    AlphaConvert(root.AbsVarName!, root.AbsBody!) is var (newVar, newBody)
                        ? Expr.Abs(newVar, Substitute(newBody, var, val))
                        : throw new InvalidOperationException("Alpha conversion failed"),

                ExprType.Abs => Expr.Abs(root.AbsVarName!, Substitute(root.AbsBody!, var, val)),

                ExprType.App when root.AppLeft != null && root.AppRight != null =>
                    CreateOptimizedApplication(root.AppLeft, root.AppRight, var, val),

                ExprType.YCombinator => root, // Y combinator doesn't contain variables

                _ => throw new InvalidOperationException($"Unknown expression type: {root.Type}")
            };
        }

        _stats.VarCounter--;
        _stats.TimeInSubstitution += _perfStopwatch.ElapsedTicks;

        PutSubCache(root, var, val, result);

        return result;
    }

    private bool IsCommonPattern(Expr app, string var, Expr val, out Expr result)
    {
        result = null!;

        if (app.AppLeft?.Type != ExprType.Var || app.AppRight?.Type != ExprType.Var)
            return false; // Not a simple application

        var (result1, flag) = (app.AppLeft.VarName == var, app.AppRight.VarName == var) switch
        {
            (true, true) => (Expr.App(val, val), true), // Pattern: (var var) -> val val
            (true, false) => (Expr.App(val, app.AppRight), true), // Pattern: (var x) -> val x
            (false, true) => (Expr.App(app.AppLeft, val), true), // Pattern: (f x) where f doesn't contain var and x is var -> f val
            _ => (app, false) // Neither side contains the variable
        };
        result = result1;
        return flag;
    }

    private bool QuickFreeVarCheck(Expr expr, string varName)
    {
        if (_freeVarCache.TryGetValue(expr, out var freeVars))
            return freeVars.Contains(varName);

        // For simple expressions, do direct check
        return expr.Type switch
        {
            ExprType.Var => expr.VarName == varName,
            ExprType.Abs => expr.AbsVarName != varName && QuickFreeVarCheck(expr.AbsBody!, varName),
            ExprType.Thunk => expr.ThunkValue is not null && QuickFreeVarCheck(expr.ThunkValue.Expression, varName),
            _ => FreeVars(expr).Contains(varName),// Fall back to full computation for complex cases
        };
    }

    private Expr CreateOptimizedApplication(Expr left, Expr right, string var, Expr val)
    => (ContainsVariable(left, var), ContainsVariable(right, var)) switch
    {
        (false, false) => Expr.App(left, right),
        (false, true) => Expr.App(left, Substitute(right, var, val)),
        (true, false) => Expr.App(Substitute(left, var, val), right),
        (true, true) => Expr.App(Substitute(left, var, val), Substitute(right, var, val))
    };

    private bool ContainsVariable(Expr expr, string var)
    {
        if (expr.Type == ExprType.Var)
            return expr.VarName == var;

        var key = (expr, var);
        if (_containsVarCache.TryGetValue(key, out var cached))
            return cached;

        var stack = new Stack<Expr>();
        stack.Push(expr);
        while (stack.Count > 0)
        {
            var current = stack.Pop();
            switch (current.Type)
            {
                case ExprType.Var:
                    if (current.VarName == var)
                    {
                        _containsVarCache[key] = true;
                        return true;
                    }
                    break;

                case ExprType.Abs when current.AbsBody != null:
                    stack.Push(current.AbsBody);
                    break;

                case ExprType.App:
                    if (current.AppRight != null) stack.Push(current.AppRight);
                    if (current.AppLeft != null) stack.Push(current.AppLeft);
                    break;

                case ExprType.Thunk when current.ThunkValue != null:
                    stack.Push(current.ThunkValue.Expression);
                    break;

                case ExprType.YCombinator:
                    // Y combinator has no variables
                    break;
            }
        }
        _containsVarCache[key] = false;
        return false;
    }

    private Expr SubstituteStackBased(Expr root, string var, Expr val)
    {
        var opStack = new Stack<StackEntry>(64);
        var resultStack = new Stack<Expr>(64);

        var (currentVar, currentVal) = (var, val);

        opStack.Push(new StackEntry(root, SubstOp.Evaluate, null));

        while (opStack.Count > 0)
        {
            var entry = opStack.Pop();
            var (node, op, extra) = (entry.Node, entry.Op, entry.Extra);

            switch (op)
            {
                case SubstOp.Evaluate when node.Type == ExprType.Var:
                    if (_showStep && node.VarName == currentVar)
                        _logger.Log($"Step: Substitute variable \t{currentVar} with {FormatWithNumerals(currentVal)}");
                    resultStack.Push(node.VarName == currentVar ? currentVal : Intern(node));
                    continue;

                case SubstOp.Evaluate when node.Type == ExprType.Abs && node.AbsVarName == currentVar:
                    // Fast path for shadowed variables
                    resultStack.Push(node);
                    continue;

                case SubstOp.Evaluate when node.Type == ExprType.Thunk:
                    // Handle thunk substitution
                    if (node.ThunkValue is not null)
                    {
                        if (_showStep)
                            _logger.Log($"Step: Substitute in thunk: \t{FormatWithNumerals(node.ThunkValue.Expression)}");
                        var substitutedExpr = Substitute(node.ThunkValue.Expression, currentVar, currentVal);
                        var newThunk = new Thunk(substitutedExpr, node.ThunkValue.Environment);
                        if (node.ThunkValue.IsForced)
                            newThunk.Force(node.ThunkValue.ForcedValue!);
                        resultStack.Push(node with { ThunkValue = newThunk });
                    }
                    else
                        resultStack.Push(node);
                    continue;

                case SubstOp.Evaluate:
                    switch (node.Type)
                    {
                        case ExprType.Abs:
                            var (absVarName, absBody) = (node.AbsVarName!, node.AbsBody!);
                            if (currentVal.Type != ExprType.Var && absVarName != currentVar && FreeVars(currentVal).Contains(absVarName))
                            {
                                // Alpha conversion needed - use block scope to avoid conflicts
                                string newVar = absVarName + _stats.VarCounter++;

                                // Setup operations in reverse order
                                opStack.Push(new StackEntry(node, SubstOp.BuildAbs, newVar));
                                opStack.Push(new StackEntry(null!, SubstOp.SubstituteInBody, (currentVar, currentVal)));
                                opStack.Push(new StackEntry(absBody, SubstOp.Evaluate, null));

                                // Perform variable renaming
                                (currentVar, currentVal) = (absVarName, Expr.Var(newVar));
                                if (_showStep)
                                    _logger.Log($"Step: Alpha-convert \t{absVarName} to {newVar}");
                            }
                            else
                            {
                                // No alpha conversion needed
                                opStack.Push(new StackEntry(node, SubstOp.BuildAbs, absVarName));
                                opStack.Push(new StackEntry(absBody, SubstOp.Evaluate, null));
                            }
                            break;

                        case ExprType.App:
                            // Process application - results will be popped in reverse order
                            opStack.Push(new StackEntry(node, SubstOp.BuildApp, null));
                            opStack.Push(new StackEntry(node.AppRight!, SubstOp.Evaluate, null));
                            opStack.Push(new StackEntry(node.AppLeft!, SubstOp.Evaluate, null));
                            break;

                        case ExprType.YCombinator:
                            // Y combinator has no variables, so substitution returns itself
                            resultStack.Push(node);
                            break;
                    }
                    break;

                case SubstOp.BuildAbs:
                    var newBody = resultStack.Pop();
                    var varName = (string)extra!;
                    resultStack.Push(Expr.Abs(varName, newBody));
                    break;

                case SubstOp.BuildApp:
                    var right = resultStack.Pop();
                    var left = resultStack.Pop();
                    resultStack.Push(Expr.App(left, right));
                    break;
                    
                case SubstOp.SubstituteInBody:
                    // Restore previous substitution context
                    (currentVar, currentVal) = ((string, Expr))extra!;
                    break;
            }
        }

        return resultStack.Pop();
    }

    private Expr NormalizeExpression(Expr expr)
    {
        if (_normalizationCache.TryGetValue(expr, out var cached))
            return cached;
        var visited = new HashSet<Expr>();
        var result = NormalizeWithVisited(expr, visited, 0, 1000); // Max depth of 1000
        _normalizationCache[expr] = result;
        return result;
    }

    private Expr NormalizeWithVisited(Expr expr, HashSet<Expr> visited, int depth, int maxDepth)
    {
        _stats.NormalizeCEKCount++;
        if (_normalizationCache.TryGetValue(expr, out var cached))
            return cached;
        if (depth > maxDepth || visited.Contains(expr))
            return expr;

        visited.Add(expr);
        try
        {
            Expr result = expr.Type switch
            {
                ExprType.Var => expr,
                ExprType.Abs => Expr.Abs(expr.AbsVarName!, NormalizeWithVisited(expr.AbsBody!, visited, depth + 1, maxDepth)),
                ExprType.App => NormalizeApplicationWithVisited(expr, visited, depth, maxDepth),
                ExprType.Thunk => Force(expr), // Force thunks during normalization
                ExprType.YCombinator => expr, // Y combinator itself is normalized, but applications should be handled in NormalizeApplicationWithVisited
                _ => expr
            };
            _normalizationCache[expr] = result;
            return result;
        }
        finally
        {
            visited.Remove(expr);
        }
    }

    private Expr NormalizeApplicationWithVisited(Expr app, HashSet<Expr> visited, int depth, int maxDepth)
    {
        if (depth > maxDepth)
            return app;

        var left = NormalizeWithVisited(app.AppLeft!, visited, depth + 1, maxDepth);
        var right = NormalizeWithVisited(app.AppRight!, visited, depth + 1, maxDepth);

        // If left side is a lambda, perform beta reduction
        if (left.Type == ExprType.Abs)
        {
            var substituted = Substitute(left.AbsBody!, left.AbsVarName!, right);
            // Only continue normalizing if we haven't hit recursion limits
            if (depth < maxDepth - 5)
                return NormalizeWithVisited(substituted, visited, depth + 1, maxDepth);
            return substituted;
        }

        return Expr.App(left, right);
    }

    // Returns the integer value of a Church numeral (λf.λx.f^n(x)), or null if not valid.
    private static int? ExtractChurchNumeralValue(Expr expr)
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

    // Check if an expression is Church true: λx.λy.x
    private static bool IsChurchTrue(Expr expr) =>
        expr is
        {
            Type: ExprType.Abs,
            AbsVarName: var x,
            AbsBody:
            {
                Type: ExprType.Abs,
                AbsVarName: var y,
                AbsBody: { Type: ExprType.Var, VarName: var x2 }
            }
        } && x == x2;

    // Handle Church conditional with lazy evaluation of branches
    private void HandleChurchConditional(Expr control, Dictionary<string, Expr> env, Kontinuation kont, Stack<CEKState> stateStack)
    {
        // Extract: App(App(App(if, condition), then_branch), else_branch)
        var outerApp = control; // App(App(App(if, condition), then_branch), else_branch)
        var elseExpr = outerApp.AppRight!;
        
        var middleApp = outerApp.AppLeft!; // App(App(if, condition), then_branch)
        var thenExpr = middleApp.AppRight!;
        
        var innerApp = middleApp.AppLeft!; // App(if, condition)
        var conditionExpr = innerApp.AppRight!;
        
        // Evaluate the condition first
        var conditionalKont = Kontinuation.Conditional(thenExpr, elseExpr, env, kont);
        stateStack.Push(new CEKState(conditionExpr, env, conditionalKont));
    }

    // Resolve a variable to its value in the environment
    private Expr ResolveVariable(Expr expr, Dictionary<string, Expr> env)
    {
        if (expr is { Type: ExprType.Var, VarName: var varName } && varName != null && env.TryGetValue(varName, out var value))
            return value;
        return expr;
    }
}

public class Program
{
    public static async Task Main(string[] args)
    {
        Console.OutputEncoding = System.Text.Encoding.UTF8;
        Console.InputEncoding = System.Text.Encoding.UTF8;

        var interpreter = new Interpreter(logger: new());

        await interpreter.LoadFileIfExistsAsync("stdlib.lambda");

        foreach (var filePath in args)
        {
            var result = await interpreter.LoadFileIfExistsAsync(filePath);
            if (result.StartsWith("File not found:"))
                Console.WriteLine(result);
        }

        Logger.LogToConsole("");
        Logger.LogToConsole("Lambda Calculus Interpreter - Interactive Mode");
        Logger.LogToConsole("Type ':help' for a list of commands or ':exit' to quit");
        await interpreter.RunInteractiveLoopAsync();
    }
}