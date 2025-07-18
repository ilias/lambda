namespace LambdaCalculus;

public enum ExprType : byte { Var, Abs, App, Thunk, Let, YCombinator }

// Thunk represents a delayed computation (mutable for in-place update).
public class Thunk(Expr expr, Dictionary<string, Expr> env)
{
    public Expr Expression { get; } = expr;
    public Dictionary<string, Expr> Environment { get; } = env;
    public bool IsForced { get; private set; } = false;
    public Expr? ForcedValue { get; private set; } = null;

    public void Force(Expr value)
    {
        IsForced = true;
        ForcedValue = value;
    }
}

public record Expr(
    ExprType Type,
    string? VarName = null,
    string? AbsVarName = null, Expr? AbsBody = null,
    Expr? AppLeft = null, Expr? AppRight = null,
    Thunk? ThunkValue = null,
    string? LetVarName = null, Expr? LetValue = null, Expr? LetBody = null)
{
    public static int HashCodeCount { get; private set; }
    private int? _hashCode;
    public static Expr Var(string name) => new(ExprType.Var, VarName: name);
    public static Expr Abs(string name, Expr body) => new(ExprType.Abs, AbsVarName: name, AbsBody: body);
    public static Expr App(Expr left, Expr right) => new(ExprType.App, AppLeft: left, AppRight: right);
    public static Expr Thunk(Expr expr, Dictionary<string, Expr> env) => new(ExprType.Thunk, ThunkValue: new Thunk(expr, env));
    public static Expr Let(string varName, Expr value, Expr body) => new(ExprType.Let, LetVarName: varName, LetValue: value, LetBody: body);
    public static Expr YCombinator() => new(ExprType.YCombinator);
    public override string ToString() => ToStringLimited(1000);
    private string ToStringLimited(int maxDepth) =>
        maxDepth <= 0 ? "..." : Type switch
        {
            ExprType.Var => VarName!,
            ExprType.Abs => $"λ{AbsVarName}.{AbsBody?.ToStringLimited(maxDepth - 1) ?? "null"}",
            ExprType.App => FormatApplication(maxDepth),
            ExprType.Thunk => ThunkValue!.IsForced ?
                $"<forced:{ThunkValue.ForcedValue?.ToStringLimited(maxDepth - 1) ?? "null"}>" :
                $"<thunk:{ThunkValue.Expression.ToStringLimited(maxDepth - 1)}>",
            ExprType.Let => $"let {LetVarName} = {LetValue?.ToStringLimited(maxDepth - 1) ?? "null"} in {LetBody?.ToStringLimited(maxDepth - 1) ?? "null"}",
            ExprType.YCombinator => "Y",
            _ => "?"
        };
    private string FormatApplication(int maxDepth)
    {
        var leftStr = AppLeft!.Type == ExprType.Abs
            ? $"({AppLeft.ToStringLimited(maxDepth - 1)})"
            : AppLeft.ToStringLimited(maxDepth - 1);
        var rightStr = AppRight!.Type is ExprType.App or ExprType.Abs
            ? $"({AppRight.ToStringLimited(maxDepth - 1)})"
            : AppRight.ToStringLimited(maxDepth - 1);
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
                case ExprType.Let when left.LetVarName != right.LetVarName: return false;
                case ExprType.Let when left.LetValue is null && right.LetValue is null && left.LetBody is null && right.LetBody is null: continue;
                case ExprType.Let when left.LetValue is null || right.LetValue is null || left.LetBody is null || right.LetBody is null: return false;
                case ExprType.Let:
                    stack.Push((left.LetBody!, right.LetBody!));
                    stack.Push((left.LetValue!, right.LetValue!));
                    break;
                case ExprType.YCombinator:
                    break; // Y combinators are always equal to other Y combinators
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
                case ExprType.Let when (current.LetValue is null || current.LetValue._hashCode.HasValue) &&
                                       (current.LetBody is null || current.LetBody._hashCode.HasValue):
                    current._hashCode = HashCode.Combine(current.Type, current.LetVarName,
                        current.LetValue?._hashCode ?? 0, current.LetBody?._hashCode ?? 0);
                    break;
                case ExprType.Let:
                    stack.Push(current);
                    if (current.LetValue is not null) stack.Push(current.LetValue);
                    if (current.LetBody is not null) stack.Push(current.LetBody);
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

public enum KontinuationType : byte { Empty, Arg, Fun, Let, Conditional }
public record Kontinuation(KontinuationType Type, Expr? Expression = null,
    Dictionary<string, Expr>? Environment = null, Expr? Value = null, Kontinuation? Next = null,
    string? VarName = null, Expr? ThenBranch = null, Expr? ElseBranch = null)
{
    public static readonly Kontinuation Empty = new(KontinuationType.Empty);

    public static Kontinuation Arg(Expr expr, Dictionary<string, Expr> env, Kontinuation next) =>
        new(KontinuationType.Arg, expr, env, Next: next);

    public static Kontinuation Fun(Expr value, Kontinuation next) =>
        new(KontinuationType.Fun, Value: value, Next: next);

    public static Kontinuation Let(string varName, Expr body, Dictionary<string, Expr> env, Kontinuation next) =>
        new(KontinuationType.Let, body, env, Next: next, VarName: varName);

    public static Kontinuation Conditional(Expr thenBranch, Expr elseBranch, Dictionary<string, Expr> env, Kontinuation next) =>
        new(KontinuationType.Conditional, Environment: env, Next: next, ThenBranch: thenBranch, ElseBranch: elseBranch);
}
public record CEKState(Expr Control, Dictionary<string, Expr> Environment, Kontinuation Kontinuation);

public enum TokenType : byte { LParen, RParen, Lambda, Term, Equals, Integer, LBracket, RBracket, Comma, Let, In, Dot, Y }
public record Token(TokenType Type, int Position, string? Value = null);
public enum TreeErrorType : byte { UnclosedParen, UnopenedParen, MissingLambdaVar, MissingLambdaBody, EmptyExprList, IllegalAssignment }
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

public class Parser
{
    TokenType MyTokenType(string? term) => term switch
    {
        "let" => TokenType.Let,
        "in" => TokenType.In,
        "Y" => TokenType.Y,
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

            Token? nextToken = ch switch
            {
                '\\' or 'λ' => new Token(TokenType.Lambda, pos),
                '(' => new Token(TokenType.LParen, pos),
                ')' => new Token(TokenType.RParen, pos),
                '[' => new Token(TokenType.LBracket, pos),
                ']' => new Token(TokenType.RBracket, pos),
                ',' => new Token(TokenType.Comma, pos),
                '=' => new Token(TokenType.Equals, pos),
                '.' => new Token(TokenType.Dot, pos),
                char c when char.IsDigit(c) && currentTerm.Length == 0 => ParseInteger(input, ref i, ref pos),
                _ => null
            };

            if (nextToken is null && !char.IsWhiteSpace(ch))
            {
                currentTerm.Append(ch);
            }
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

        // Assignment: name = expr
        if (tokens.Count > 2 && tokens[0].Type == TokenType.Term && tokens[1].Type == TokenType.Equals)
            return Statement.AssignmentStatement(
                tokens[0].Value!,
                BuildExpressionTree(tokens, 2, tokens.Count - 1));

        // Expression statement
        return Statement.ExprStatement(BuildExpressionTree(tokens, 0, tokens.Count - 1));
    }
    private Expr BuildExpressionTree(List<Token> tokens, int start, int end)
    {
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
                TokenType.Equals => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position),
                _ => throw new ParseException(TreeErrorType.EmptyExprList, token.Position)
            });
        }
        return expressions.Count == 0
            ? throw new ParseException(TreeErrorType.EmptyExprList, tokens.Count > 0 ? tokens[0].Position : 0)
            : expressions.Aggregate(Expr.App);
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
    private Expr ParseLambdaExpr(List<Token> tokens, ref int i, int end)
    {
        var variables = new List<string>();
        i++; // Skip λ or \
        
        // Collect all variables before the dot
        while (i <= end && tokens[i].Type == TokenType.Term)
        {
            variables.Add(tokens[i].Value!);
            i++;
        }
        
        if (variables.Count == 0)
            throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[i - 1].Position);
        
        // Check for dot separator
        if (i > end || tokens[i].Type != TokenType.Dot)
        {
            // No dot found - fall back to old behavior for single variable
            if (variables.Count == 1)
            {
                i--; // Go back to process the body starting from current position
                var body = BuildExpressionTree(tokens, i, end);
                i = end;
                return Expr.Abs(variables[0], body);
            }
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
            
            // Find the end of this element (up to comma or closing bracket)
            var elementEnd = i;
            var nesting = 0;
            
            while (elementEnd <= end)
            {
                var token = tokens[elementEnd];
                if (token.Type == TokenType.LParen || token.Type == TokenType.LBracket)
                    nesting++;
                else if (token.Type == TokenType.RParen || token.Type == TokenType.RBracket)
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
    private Expr ParseLetExpr(List<Token> tokens, ref int i, int end)
    {
        // let var = value in body
        if (i + 4 > end)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i].Position);
        
        if (tokens[i + 1].Type != TokenType.Term)
            throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[i].Position);
        
        var varName = tokens[i + 1].Value!;
        
        if (tokens[i + 2].Type != TokenType.Equals)
            throw new ParseException(TreeErrorType.IllegalAssignment, tokens[i + 2].Position);
        
        // Find the "in" keyword
        var inPos = -1;
        var nesting = 0;
        for (var j = i + 3; j <= end; j++)
        {
            var token = tokens[j];
            if (token.Type == TokenType.LParen || token.Type == TokenType.LBracket)
                nesting++;
            else if (token.Type == TokenType.RParen || token.Type == TokenType.RBracket)
                nesting--;
            else if (nesting == 0 && token.Type == TokenType.In)
            {
                inPos = j;
                break;
            }
        }
        
        if (inPos == -1)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i].Position);
        
        var value = BuildExpressionTree(tokens, i + 3, inPos - 1);
        var body = BuildExpressionTree(tokens, inPos + 1, end);
        
        i = end;
        return Expr.Let(varName, value, body);
    }
}

public class Logger
{
    private string _logFile = string.Empty;
    private StreamWriter? _logWriter;
    private readonly SemaphoreSlim _logFileLock = new(1);

    private static readonly Dictionary<string, string> _colors = new(StringComparer.Ordinal)
    {
        ["red"] = "\u001b[31m",
        ["green"] = "\u001b[32m",
        ["yellow"] = "\u001b[33m",
        ["blue"] = "\u001b[34m",
        ["magenta"] = "\u001b[35m",
        ["cyan"] = "\u001b[36m",
        ["white"] = "\u001b[37m",
        ["gray"] = "\u001b[90m",
        ["reset"] = "\u001b[0m"
    };

    public static string Prompt(string txt) => $"{_colors["cyan"]}{txt}{_colors["reset"]} ";
    public string LogStatus => _logFile == string.Empty ? "DISABLED" : _logFile;

    public async Task<string> HandleLogCommandAsync(string arg) => arg switch
    {
        "off" or "" => (_logFile = string.Empty, "Logging is disabled.").Item2,
        "clear" => await ClearLogFileAsync(),
        _ => (_logFile = arg, $"Logging is enabled to '{arg}'").Item2
    };

    public async Task<string> ClearLogFileAsync()
    {
        try
        {
            await CloseLogFileAsync();
            await File.WriteAllTextAsync(_logFile, string.Empty);
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

    public static string GetColor(string message) => message switch
    {
        string s when s.StartsWith("Error:") => _colors["red"],
        string s when s.StartsWith("#") => _colors["yellow"], // Comments
        string s when s.StartsWith("->") => _colors["green"], // Results/Assignments
        string s when s.StartsWith("Step") => _colors["yellow"], // Evaluation steps
        string s when s.StartsWith("Time:") => _colors["magenta"], // Timing info
        string s when s.StartsWith("Result ") => _colors["magenta"], // Final result details
        string s when s.Contains("Loading") => _colors["cyan"], // loading files
        string s when s.Contains("<<") => _colors["gray"], // reading file lines
        string s when s.Contains(">>") => _colors["green"], // result of reading file lines
        _ => _colors["reset"] // Default
    };

    public static void LogToConsole(string message) =>
        Console.WriteLine(GetColor(message) + message.Replace("\t", _colors["reset"]) + _colors["reset"]);

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
        LogAsync(message, toConsole).GetAwaiter().GetResult();
}

// Used for stack-based substitution in Interpreter
internal enum SubstOp { Evaluate, BuildAbs, BuildApp, BuildLet, SubstituteInBody }
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

    public Interpreter(Logger logger, InterpreterStats? stats = null)
    {
        _logger = logger;
        _stats = stats ?? new InterpreterStats();
    }

    public async Task<(Expr? exp, string str)> ProcessInputAsync(string input)
    {
        try
        {
            _stats.Iterations = 0; // Reset iterations for each input
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
                return (null, $"-> {statement.VarName} = {evaluatedExpression}");
            }
            var result = EvaluateCEK(statement.Expression);
            _stats.TotalIterations += _stats.Iterations; 

            return (result, $"-> {result}");
        }
        catch (Exception ex)
        {
            return (null, $"Error: {ex.Message}");
        }
    }

    private async Task DisplayOutput((Expr? expr, string str) result, TimeSpan elapsed)
    {
        if (result.expr is not null)
        {
            var number = ExtractChurchNumeralValue(result.expr);
            string? resultName = _context.FirstOrDefault(kv => kv.Value.StructuralEquals(result.expr)).Key;

            if (number is not null || !string.IsNullOrEmpty(resultName))
                await _logger.LogAsync("Result is" +
                    (number is not null ? $" (Church numeral {number:#,##0})" : "") +
                    (!string.IsNullOrEmpty(resultName) ? $" (named '{resultName}')" : ""));

            var timeInfo = elapsed.TotalSeconds >= 1
                ? $"{elapsed.TotalSeconds:F2} s"
                : $"{elapsed.TotalMilliseconds:F1} ms";
            await _logger.LogAsync($"Time: {timeInfo}, iterations: {_stats.Iterations:#,##0}");
        }

        await _logger.LogAsync(result.str);
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
        const int maxIterations = 100000; // Prevent infinite loops
        
        while (stateStack.Count > 0 && _stats.Iterations < maxIterations)
        {
            _stats.Iterations++;
            var (control, env, kont) = stateStack.Pop();
            if (_showStep)
                _logger.Log($"Step {currentStep++}: CEK \tC: {control}, K: {kont.Type}");
            if (GetEvalCache(control, out var cachedResult))
            {
                ApplyContinuation(cachedResult, env, kont, stateStack, ref finalResult);
                continue;
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
                case { Type: ExprType.Let, LetVarName: var letVar, LetValue: var letValue, LetBody: var letBody }:
                    // Create a continuation for the let binding
                    var letKont = Kontinuation.Let(letVar!, letBody!, env, kont);
                    stateStack.Push(new CEKState(letValue!, env, letKont));
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
            var result = NormalizeExpression(finalResult);
            PutEvalCache(currentStep, expr, result);
            return result;
        }
        throw new InvalidOperationException("CEK evaluation completed without returning a value");
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
    {
        if (expr is not null && _evaluationCache.TryGetValue(expr, out var cachedResult) && cachedResult is not null)
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
    public async Task<string> LoadFileAsync(string path)
    {
        int lineCount = 0;
        _logger.Log($"Loading commands from '{path}'");
        var lines = await File.ReadAllLinesAsync(path);
        var currentInput = new System.Text.StringBuilder();
        foreach (var line in lines)
        {
            await _logger.LogAsync($"line {lineCount++} <<: {line}");
            var trimmed = line.TrimEnd();
            if (string.IsNullOrWhiteSpace(trimmed) || trimmed.StartsWith('#'))
                continue;
            if (trimmed.EndsWith('\\'))
            {
                currentInput.Append(trimmed[..^1]);
                continue;
            }
            currentInput.Append(trimmed);
            var input = currentInput.ToString();
            currentInput.Clear();
            await ProcessAndDisplayInputAsync(input);
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
            _ => $"Unknown command: {command}"
        };
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

    // Force evaluation of a thunk (lazy value)
    private Expr Force(Expr expr)
    {
        if (expr.Type != ExprType.Thunk || expr.ThunkValue is null)
            return expr;

        if (expr.ThunkValue.IsForced)
            return expr.ThunkValue.ForcedValue!;

        _perfStopwatch.Restart();
        _stats.ThunkForceCount++;

        // Evaluate the thunk's expression in its captured environment
        var forced = EvaluateCEK(expr.ThunkValue.Expression, expr.ThunkValue.Environment);
        
        // Recursively force the result until we get a non-thunk
        while (forced.Type == ExprType.Thunk && forced.ThunkValue != null && !forced.ThunkValue.IsForced)
        {
            forced = EvaluateCEK(forced.ThunkValue.Expression, forced.ThunkValue.Environment);
        }
        
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
            _logger.Log($"  {key} = {value}");
        return $"# Displayed {_context.Count} definitions.";
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
        Definitions:              {_context.Count:#,##0}
        Recursion depth limit:    {_stats.MaxRecursionDepth:#,##0}
        Unique var counter:       {_stats.VarCounter:#,##0}
        
        -- Evaluation --
        Mode:                     {evalMode}
        Step-by-step:             {(_showStep ? "ENABLED" : "DISABLED")}
        Normalizations:           {_stats.NormalizeCEKCount:#,##0}
        Thunks forced:            {_stats.ThunkForceCount:#,##0}
        Total iterations:         {_stats.TotalIterations:#,##0}
        Hash code calls:          {Expr.HashCodeCount:#,##0}
        
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
          (expr)                 Grouping (e.g., (\x.x) y)
          expr1 expr2            Application (e.g., succ 0)
          name = expr            Assignment (e.g., id = \x.x)
          123                    Integer literal (Church numeral λf.λx.f^n(x))
          [a, b, c]              List literal (cons a (cons b (cons c nil)))
          let x = e1 in e2       Let binding (e.g., let x = 5 in add x 3)
          Y f1                   Y combinator (e.g., Y \f.\x.f (f x))

        -- Commands (prefix with ':') --
          :load <file>           Load definitions from file (e.g., :load stdlib.lambda)
          :save <file>           Save current environment to file (e.g., :save myenv.lambda)
          :log <file|off>        Log output to file or disable logging (e.g., :log session.log, :log off)
          :log clear             Clear the current log file (if enabled)
          :step on|off           Toggle step-by-step evaluation logging
          :lazy on|off           Toggle lazy evaluation (default: on) or (eager evaluation)
          :stats                 Show detailed performance and environment statistics
          :depth [n]             Set/show max recursion depth (default: 100, range: 10-10000)
          :env                   Show current environment definitions
          :help                  Show this help message
          :memo                  Clear all memoization/caches
          :exit, :quit           Exit the interpreter

        -- Interactive Features --
          - Line continuation: Use '\' at end of line to continue input
          - Comments: Lines starting with '#' are ignored, or any text after '#' in a line is ignored
          - Command line arguments: Treated as files to load at startup
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

                if (function!.Type == ExprType.Abs)
                {
                    // Beta reduction - substitute argument for parameter in function body
                    var substituted = Substitute(function.AbsBody!, function.AbsVarName!, argument);
                    stateStack.Push(new CEKState(substituted, env, next!));
                }
                else if (function.Type == ExprType.YCombinator)
                {
                    // Y combinator using call-by-value Z combinator approach
                    // The issue with the direct Y f = f (Y f) is infinite expansion.
                    // 
                    // The Z combinator solves this by delaying the self-application:
                    // Z = λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))
                    //
                    // This creates the recursive structure without immediate expansion.
                    
                    var x1 = $"__x1_{Guid.NewGuid():N}"[..12];
                    var x2 = $"__x2_{Guid.NewGuid():N}"[..12];
                    var v1 = $"__v1_{Guid.NewGuid():N}"[..12];
                    var v2 = $"__v2_{Guid.NewGuid():N}"[..12];
                    
                    // Create: λv. x x v
                    var selfApp1 = Expr.App(Expr.Var(x1), Expr.Var(x1));
                    var delayed1 = Expr.Abs(v1, Expr.App(selfApp1, Expr.Var(v1)));
                    
                    // Create: f (λv. x x v)
                    var fApplied1 = Expr.App(argument, delayed1);
                    
                    // Create: λx. f (λv. x x v)
                    var leftSide = Expr.Abs(x1, fApplied1);
                    
                    // Create second copy: λv. x x v
                    var selfApp2 = Expr.App(Expr.Var(x2), Expr.Var(x2));
                    var delayed2 = Expr.Abs(v2, Expr.App(selfApp2, Expr.Var(v2)));
                    
                    // Create: f (λv. x x v)
                    var fApplied2 = Expr.App(argument, delayed2);
                    
                    // Create: λx. f (λv. x x v)
                    var rightSide = Expr.Abs(x2, fApplied2);
                    
                    // Apply: (λx. f (λv. x x v)) (λx. f (λv. x x v))
                    var result = Expr.App(leftSide, rightSide);
                    
                    // Push the result for further evaluation
                    stateStack.Push(new CEKState(result, env, next!));
                }
                else
                {
                    // Not a function - create application and continue with next continuation
                    // This prevents infinite loops with undefined variables
                    var app = Expr.App(function, argument);
                    ApplyContinuation(app, env, next!, stateStack, ref finalResult);
                }
                break;

            case { Type: KontinuationType.Let, VarName: var varName, Expression: var body, Environment: var letEnv, Next: var next }:
                // We have evaluated the let value, now add it to environment and evaluate the body
                var newEnv = new Dictionary<string, Expr>(letEnv!, StringComparer.Ordinal) { [varName!] = value };
                stateStack.Push(new CEKState(body!, newEnv, next!));
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

                case ExprType.Let when current.LetVarName is not null && current.LetValue is not null && current.LetBody is not null:
                    var letVar = current.LetVarName;
                    // First process the value (where letVar is not bound)
                    stack.Push((current.LetValue, true, null));
                    // Then process the body (where letVar is bound)
                    boundVars.Add(letVar);
                    stack.Push((current, false, letVar)); // Cleanup entry
                    stack.Push((current.LetBody, true, null));
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
        var hasResult = _substitutionCache.TryGetValue(cacheKey, out Expr? result);

        _ = hasResult ? _stats.CacheHits++ : _stats.CacheMisses++;
        return result;
    }

    private void PutSubCache(Expr root, string var, Expr val, Expr result)
        => _substitutionCache[new SubstitutionCacheKey(root, var, val)] = result;

    private Expr Substitute(Expr root, string var, Expr val)
    {
        // Fast-path: If substituting a variable with itself, return the original expression
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
            if (root.VarName == var) return val;

            if (_expressionPool.TryGetValue(root.VarName!, out var cached)) return cached;

            // Cache all variable lookups for better performance
            var toCache = Intern(root);
            _expressionPool[root.VarName!] = toCache;
            return toCache;
        }

        // Fast path for abstractions that shadow the variable
        if (root.Type == ExprType.Abs && root.AbsVarName == var) return root;
        
        // Fast path for let expressions that shadow the variable
        if (root.Type == ExprType.Let && root.LetVarName == var) 
            return Expr.Let(root.LetVarName!, Substitute(root.LetValue!, var, val), root.LetBody!);

        // Handle thunks
        if (root.Type == ExprType.Thunk && root.ThunkValue is not null)
        {
            var substitutedExpr = Substitute(root.ThunkValue.Expression, var, val);
            var newThunk = new Thunk(substitutedExpr, root.ThunkValue.Environment);
            if (root.ThunkValue.IsForced)
                newThunk.Force(root.ThunkValue.ForcedValue!);
            return root with { ThunkValue = newThunk };
        }

        // Ultra-fast application patterns for common Church numeral operations
        if (root.Type == ExprType.App && IsCommonPattern(root, var, val, out var fastResult)) return fastResult;

        _perfStopwatch.Restart();

        Expr result;
        if (++_stats.VarCounter > _stats.MaxRecursionDepth)
            result = SubstituteStackBased(root, var, val);
        else
        {
            result = root.Type switch
            {
                ExprType.Abs when val.Type != ExprType.Var &&
                                  root.AbsVarName != var &&
                                  QuickFreeVarCheck(val, root.AbsVarName!) =>
                    AlphaConvert(root.AbsVarName!, root.AbsBody!) is var (newVar, newBody)
                        ? Expr.Abs(newVar, Substitute(newBody, var, val))
                        : throw new InvalidOperationException("Alpha conversion failed"),

                ExprType.Abs => Expr.Abs(root.AbsVarName!, Substitute(root.AbsBody!, var, val)),

                ExprType.App when root.AppLeft != null && root.AppRight != null =>
                    CreateOptimizedApplication(root.AppLeft, root.AppRight, var, val),

                ExprType.Let when val.Type != ExprType.Var &&
                                  root.LetVarName != var &&
                                  QuickFreeVarCheck(val, root.LetVarName!) =>
                    AlphaConvert(root.LetVarName!, root.LetBody!) is var (newVar, newBody)
                        ? Expr.Let(newVar, Substitute(root.LetValue!, var, val), Substitute(newBody, var, val))
                        : throw new InvalidOperationException("Alpha conversion failed"),

                ExprType.Let => Expr.Let(root.LetVarName!, 
                    Substitute(root.LetValue!, var, val),
                    root.LetVarName == var ? root.LetBody! : Substitute(root.LetBody!, var, val)),

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
            ExprType.Let => QuickFreeVarCheck(expr.LetValue!, varName) || 
                           (expr.LetVarName != varName && QuickFreeVarCheck(expr.LetBody!, varName)),
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

                case ExprType.Let:
                    if (current.LetBody != null) stack.Push(current.LetBody);
                    if (current.LetValue != null) stack.Push(current.LetValue);
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

                        case ExprType.Let:
                            var (letVar, letVal, letBod) = (node.LetVarName!, node.LetValue!, node.LetBody!);
                            if (currentVal.Type != ExprType.Var && letVar != currentVar && FreeVars(currentVal).Contains(letVar))
                            {
                                // Alpha conversion needed for let variable
                                string newVar = letVar + _stats.VarCounter++;

                                // Setup operations in reverse order
                                opStack.Push(new StackEntry(node, SubstOp.BuildLet, newVar));
                                opStack.Push(new StackEntry(null!, SubstOp.SubstituteInBody, (currentVar, currentVal)));
                                opStack.Push(new StackEntry(letBod, SubstOp.Evaluate, null));
                                opStack.Push(new StackEntry(letVal, SubstOp.Evaluate, null));

                                // Perform variable renaming
                                (currentVar, currentVal) = (letVar, Expr.Var(newVar));
                            }
                            else
                            {
                                // No alpha conversion needed
                                opStack.Push(new StackEntry(node, SubstOp.BuildLet, letVar));
                                // If let variable shadows current var, don't substitute in body
                                if (letVar == currentVar)
                                    opStack.Push(new StackEntry(letBod, SubstOp.Evaluate, null)); // Don't substitute in body
                                else
                                    opStack.Push(new StackEntry(letBod, SubstOp.Evaluate, null));
                                opStack.Push(new StackEntry(letVal, SubstOp.Evaluate, null));
                            }
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
                    
                case SubstOp.BuildLet:
                    var letBodyResult = resultStack.Pop();
                    var letValueResult = resultStack.Pop();
                    var letVarName = (string)extra!;
                    resultStack.Push(Expr.Let(letVarName, letValueResult, letBodyResult));
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
                ExprType.Let => Expr.Let(expr.LetVarName!, 
                    NormalizeWithVisited(expr.LetValue!, visited, depth + 1, maxDepth),
                    NormalizeWithVisited(expr.LetBody!, visited, depth + 1, maxDepth)),
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
    private static bool IsIfFunction(Expr expr)
    {
        return expr is 
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

    // Check if an expression is Church true: λx.λy.x
    private static bool IsChurchTrue(Expr expr)
    {
        return expr is 
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
    }

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
        Console.OutputEncoding = System.Text.Encoding.UTF8; // Ensure console output supports UTF-8
        Console.InputEncoding = System.Text.Encoding.UTF8; // Ensure console input supports UTF-8

        var interpreter = new Interpreter(logger: new());

        // Load standard library if available
        if (File.Exists("stdlib.lambda"))
            await interpreter.LoadFileAsync("stdlib.lambda");

        // Process any command line files before starting interactive mode
        foreach (var filePath in args)
            if (File.Exists(filePath))
                await interpreter.LoadFileAsync(filePath);
            else
                Console.WriteLine($"File not found: {filePath}");

        Logger.LogToConsole("");
        Logger.LogToConsole("Lambda Calculus Interpreter - Interactive Mode");
        Logger.LogToConsole("Type ':help' for a list of commands or ':exit' to quit");
        await interpreter.RunInteractiveLoopAsync();
    }
}