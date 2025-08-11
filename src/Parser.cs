/*
========================================
 Minimal Grammar (Single Source of Truth)
 Generated from parser rules below
----------------------------------------
 program        ::= expr (';' expr)*                // ParseProgram
 expr           ::= letExpr | arrowExpr | infixExpr // ParseExpression
 letExpr        ::= 'let' bindings 'in' expr        // ParseLetExpr
 bindings       ::= binding (',' binding)*          // ParseLetExpr (multi-binding)
 binding        ::= name '=' expr                   // ParseLetExpr (single binding)
 arrowExpr      ::= paramList '->' expr             // ParseArrowExpr
 paramList      ::= name (',' name)* | '(' name (',' name)* ')' // IsArrowParamListAhead
 infixExpr      ::= appExpr (infixOp appExpr)*      // ParseApplication + Pratt
 appExpr        ::= primary (primary)*              // ParseApplication
 primary        ::= integer | name | '(' expr ')' | listExpr // ParsePrimary
 listExpr       ::= '[' exprList ']'                // ParseListExpr
 exprList       ::= expr (',' expr)*                // ParseListExpr
 macroDef       ::= ':macro' '(' name pattern* ')' ['when' guard] '=>' transformation // MacroExpander.ParseMacroDefinition
 pattern        ::= '$' name ['...'] | '(' pattern* ')' | name // MacroExpander.ParseMacroPattern
 guard          ::= expr                            // MacroExpander.ParseMacroGuardExpression
 transformation ::= expr                            // MacroExpander.ParseMacroTransformation
 infixOp        ::= operatorSymbol                  // Infix registry
----------------------------------------
Notes:
- Precedence: let < arrow < infix < application < atom
- All rules are left-associative unless noted in infix registry
- See README for extended grammar and desugarings
========================================
*/
namespace LambdaCalculus;

// -----------------------------------------------------------------------------
// Parser overview / informal grammar (see earlier discussion in PR notes)
// -----------------------------------------------------------------------------
public enum TokenType : byte { LParen, RParen, Lambda, Term, Equals, Integer, LBracket, RBracket, Comma, Dot, Y, Let, In, Rec, InfixOp, Arrow, Range, Macro, FatArrow, Dollar, Semicolon, Ellipsis }
public record Token(TokenType Type, int Position, string? Value = null);
public enum TreeErrorType : byte { UnclosedParen, UnopenedParen, MissingLambdaVar, MissingLambdaBody, EmptyExprList, IllegalAssignment, MissingLetVariable, MissingLetEquals, MissingLetIn, MissingLetValue, MissingLetBody, UnexpectedSemicolon, UnexpectedLambda, UnexpectedDot }
public class ParseException(TreeErrorType errorType, int position) : Exception($"{errorType} at position {position}")
{
    public TreeErrorType ErrorType { get; } = errorType;
    public int Position { get; } = position;
}

public enum StatementType : byte { Expr, Assignment }
public record Statement(StatementType Type, Expr Expression, string? VarName = null)
{
    public static Statement ExprStatement(Expr expr) => new(StatementType.Expr, expr);
    public static Statement AssignmentStatement(string varName, Expr expr) => new(StatementType.Assignment, expr, varName);
    public override string ToString() => Type == StatementType.Expr ? Expression.ToString() : $"{VarName} = {Expression}";
}

public enum Associativity : byte { Left, Right }
public record InfixOperator(string Symbol, int Precedence, Associativity Associativity, string? FunctionName = null)
{
    public string GetFunctionName() => FunctionName ?? Symbol;
}

public class Parser
{
    /// <summary>
    /// Represents a parsed parameter list (bare or parenthesized).
    /// </summary>
    public readonly struct ParameterList
    {
        public ParameterList(IReadOnlyList<string> names, int startToken, int endToken, bool parenthesized)
        {
            Names = names;
            StartToken = startToken;
            EndToken = endToken;
            Parenthesized = parenthesized;
        }
        public IReadOnlyList<string> Names { get; }
        public int StartToken { get; }
        public int EndToken { get; }
        public bool Parenthesized { get; }
    }

    /// <summary>
    /// Represents a parsed range specification (simple or stepped).
    /// </summary>
    public readonly struct RangeSpec
    {
        public RangeSpec(Expr start, Expr end, Expr? step, int startToken, int endToken)
        {
            Start = start;
            End = end;
            Step = step;
            StartToken = startToken;
            EndToken = endToken;
        }
        public Expr Start { get; }
        public Expr End { get; }
        public Expr? Step { get; }
        public int StartToken { get; }
        public int EndToken { get; }
        public bool IsStepped => Step != null;
    }
    public readonly Dictionary<string, InfixOperator> _infixOperators = new(StringComparer.Ordinal);
    public readonly Dictionary<string, List<MacroDefinition>> _macros = new(StringComparer.Ordinal);
    private readonly Tokenizer _tokenizer;
    private readonly MacroExpander _macroExpander;

    public Parser()
    {
        DefineInfixOperator("|>", 1, "left");
        DefineInfixOperator(".", 9, "right");
        _tokenizer = new Tokenizer(this);
        _macroExpander = new MacroExpander(this);
    }

    /// <summary>
    /// Builds a left-nested application chain from a sequence of expressions.
    /// Example: [f, x, y] => ((f x) y)
    /// </summary>
    public static Expr BuildApplicationChain(IEnumerable<Expr> exprs)
    {
        var list = exprs.ToList();
        if (list.Count == 0) throw new ArgumentException("Empty application chain");
        Expr result = list[0];
        for (int i = 1; i < list.Count; i++)
            result = Expr.App(result, list[i]);
        return result;
    }

    public string DefineInfixOperator(string symbol, int precedence, string associativity)
    {
        if (string.IsNullOrWhiteSpace(symbol)) return "Error: Operator symbol cannot be empty";
        if (precedence is < 1 or > 10) return "Error: Precedence must be between 1 and 10";
        if (!Enum.TryParse<Associativity>(associativity, true, out var assoc)) return "Error: Associativity must be 'left' or 'right'";
        _infixOperators[symbol] = new(symbol, precedence, assoc);
        return $"Infix operator '{symbol}' defined with precedence {precedence} and {associativity} associativity";
    }

    public bool IsInfixOperator(string symbol) => _infixOperators.ContainsKey(symbol);

    public InfixOperator? GetInfixOperator(string symbol) => _infixOperators.GetValueOrDefault(symbol);

    internal TokenType ClassifyTerm(string? term) => term switch
    {
        "Y" => TokenType.Y,
        "let" => TokenType.Let,
        "in" => TokenType.In,
        "rec" => TokenType.Rec,
        ":macro" => TokenType.Macro,
        _ when term != null && IsInfixOperator(term) => TokenType.InfixOp,
        _ => TokenType.Term
    };

    public List<Token> Tokenize(string input) => _tokenizer.Tokenize(input);

    public Statement? Parse(string input) => ParseAll(input).LastOrDefault();

    public List<Statement> ParseAll(string input)
    {
        var tokens = Tokenize(input);
        if (tokens.Count == 0) return [];

        // Split by top-level semicolons
        var segments = new List<(int start, int end)>();
        int paren = 0, bracket = 0;
        int segStart = 0;
        for (int i = 0; i < tokens.Count; i++)
        {
            var t = tokens[i];
            switch (t.Type)
            {
                case TokenType.LParen: paren++; break;
                case TokenType.RParen: paren--; break;
                case TokenType.LBracket: bracket++; break;
                case TokenType.RBracket: bracket--; break;
                case TokenType.Semicolon when paren == 0 && bracket == 0:
                    if (i - 1 >= segStart) segments.Add((segStart, i - 1));
                    segStart = i + 1; break;
            }
        }
        if (segStart < tokens.Count) segments.Add((segStart, tokens.Count - 1));

        var stmts = new List<Statement>();
        foreach (var (s, e) in segments)
        {
            if (e < s) continue;
            var slice = tokens.GetRange(s, e - s + 1);
            if (slice.Count == 0) continue;
            if (slice[0].Type == TokenType.Macro)
            {
                _macroExpander.ParseAndStoreMacroDefinition(slice);
                continue;
            }
            // Infix operator directive: :infix <symbol> <precedence> <assoc>
            if (slice.Count >= 4 && slice[0].Type == TokenType.Term && slice[0].Value == ":infix" && slice[1].Type == TokenType.Term && slice[2].Type == TokenType.Integer && slice[3].Type == TokenType.Term)
            {
                if (int.TryParse(slice[2].Value, out int prec))
                {
                    var msg = DefineInfixOperator(slice[1].Value!, prec, slice[3].Value!);
                    // Represent directive as an assignment-like statement for echoing
                    stmts.Add(Statement.ExprStatement(Expr.Var(msg.StartsWith("Error") ? "error" : "ok")));
                    continue;
                }
            }
            if (slice.Count > 2 && (slice[0].Type == TokenType.Term || slice[0].Type == TokenType.InfixOp) && slice[1].Type == TokenType.Equals)
            {
                var expr = BuildExpressionTree(slice, 2, slice.Count - 1);
                expr = ExpandMacros(expr);
                stmts.Add(Statement.AssignmentStatement(slice[0].Value!, expr));
                continue;
            }
            if (slice[0].Type == TokenType.Let)
            {
                int idx = 0;
                var letExpr = ParseLetExpr(slice, ref idx, slice.Count - 1);
                stmts.Add(Statement.ExprStatement(letExpr));
                continue;
            }
            var expr2 = BuildExpressionTree(slice, 0, slice.Count - 1);
            expr2 = ExpandMacros(expr2);
            stmts.Add(Statement.ExprStatement(expr2));
        }
        return stmts;
    }

    // ---------------- Pratt / precedence climbing core ----------------
    internal Expr BuildExpressionTree(List<Token> tokens, int start, int end)
    {
        if (start > end) throw new ParseException(TreeErrorType.EmptyExprList, start < tokens.Count ? tokens[start].Position : 0);
        int pos = start;
        var expr = ParseExpression(tokens, ref pos, end, 0);
        return expr;
    }

    private Expr ParseExpression(List<Token> tokens, ref int pos, int end, int minPrec)
    {
        Expr left;
        if (pos <= end && tokens[pos].Type == TokenType.Let)
        {
            left = ParseLetExpr(tokens, ref pos, end); // pos advanced
        }
        else if (IsArrowParamListAhead(tokens, pos, end))
        {
            // arrow param list (possibly parenthesized)
            int paramStart = pos;
            var paramNames = new List<string>();
            bool parenthesized = false;
            if (tokens[pos].Type == TokenType.LParen)
            {
                parenthesized = true;
                pos++; // skip '('
                while (pos <= end && tokens[pos].Type == TokenType.Term)
                {
                    paramNames.Add(tokens[pos].Value!);
                    pos++;
                    if (pos <= end && tokens[pos].Type == TokenType.Comma)
                    {
                        pos++;
                        continue;
                    }
                    break;
                }
                if (pos > end || tokens[pos].Type != TokenType.RParen) throw new ParseException(TreeErrorType.UnclosedParen, tokens[Math.Max(0, pos - 1)].Position);
                pos++; // skip ')'
            }
            else
            {
                while (pos <= end && tokens[pos].Type == TokenType.Term)
                {
                    paramNames.Add(tokens[pos].Value!);
                    pos++;
                    if (pos <= end && tokens[pos].Type == TokenType.Comma)
                    {
                        pos++;
                        continue;
                    }
                    break;
                }
            }
            int paramEnd = pos - 1;
            if (paramNames.Count == 0) throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[Math.Max(0, pos - 1)].Position);
            if (pos > end || tokens[pos].Type != TokenType.Arrow) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[Math.Max(0, pos - 1)].Position);
            pos++; // consume '->'
            var paramList = new ParameterList(paramNames, paramStart, paramEnd, parenthesized);
            var body = ParseExpression(tokens, ref pos, end, 0);
            left = paramList.Names.AsEnumerable().Reverse().Aggregate(body, (acc, n) => Expr.Abs(n, acc));
        }
        else
        {
            left = ParseApplication(tokens, ref pos, end);
        }

        while (pos <= end && tokens[pos].Type == TokenType.InfixOp)
        {
            var opTok = tokens[pos];
            var op = GetInfixOperator(opTok.Value!)!;
            if (op.Precedence < minPrec) break;
            pos++;
            var rhs = ParseExpression(tokens, ref pos, end, op.Precedence + (op.Associativity == Associativity.Left ? 1 : 0));
            left = DesugarInfix(op, left, rhs);
        }
        return left;
    }

    private Expr ParseApplication(List<Token> tokens, ref int pos, int end)
    {
        var exprs = new List<Expr> { ParsePrimary(tokens, ref pos, end) };
        while (pos <= end && IsPrimaryStart(tokens[pos].Type))
            exprs.Add(ParsePrimary(tokens, ref pos, end));
        return BuildApplicationChain(exprs);
    }

    // Parse a single atomic / primary expression
    private Expr ParsePrimary(List<Token> tokens, ref int pos, int end)
    {
        if (pos > end)
            throw new ParseException(TreeErrorType.EmptyExprList, pos > 0 ? tokens[pos - 1].Position : 0);
        
        var token = tokens[pos];
        return token.Type switch
        {
            TokenType.LParen => ParseParenthesizedExpr(tokens, ref pos, end),
            TokenType.LBracket => ParseListExpr(tokens, ref pos, end),
            TokenType.Lambda => ParseLambdaExpr(tokens, ref pos, end),
            TokenType.Integer => (pos++, CreateChurchNumeral(int.TryParse(token.Value, out int n) ? n : 0)).Item2,
            TokenType.Term => (pos++, Expr.Var(token.Value!)).Item2,
            TokenType.Y => (pos++, Expr.YCombinator()).Item2,
            TokenType.Let => ParseLetExpr(tokens, ref pos, end),
            _ => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position)
        };
    }

    private static bool IsPrimaryStart(TokenType t)
        => t is TokenType.LParen or TokenType.LBracket or TokenType.Lambda or TokenType.Term or TokenType.Integer or TokenType.Y or TokenType.Let;

    private static bool IsArrowParamListAhead(List<Token> tokens, int pos, int end)
    {
        if (pos > end) return false;
        // Parenthesized form: (x, y) ->
        if (tokens[pos].Type == TokenType.LParen)
        {
            int i = pos + 1;
            bool any = false;
            while (i <= end && tokens[i].Type == TokenType.Term)
            {
                any = true;
                i++;
                if (i <= end && tokens[i].Type == TokenType.Comma)
                {
                    i++;
                    continue;
                }
                break;
            }
            if (!any) return false;
            if (i > end || tokens[i].Type != TokenType.RParen) return false;
            i++;
            return i <= end && tokens[i].Type == TokenType.Arrow;
        }
        // Bare form: x, y ->
        int j = pos;
        bool anyBare = false;
        while (j <= end && tokens[j].Type == TokenType.Term)
        {
            anyBare = true;
            j++;
            if (j <= end && tokens[j].Type == TokenType.Comma)
            {
                j++;
                continue;
            }
            break;
        }
        if (!anyBare) return false;
        return j <= end && tokens[j].Type == TokenType.Arrow;
    }

    private Expr DesugarInfix(InfixOperator op, Expr left, Expr right)
    {
        if (op.Symbol == ".") return Expr.App(left, right);
        if (op.Symbol == "|>") return Expr.App(right, left);
        var f = Expr.Var(op.GetFunctionName());
        return Expr.App(Expr.App(f, left), right);
    }

    // ---------------- Atoms ----------------
    private Expr ParseParenthesizedExpr(List<Token> tokens, ref int pos, int end)
    {
        int start = pos;
        int depth = 0;
        for (int j = pos + 1; j <= end; j++)
        {
            if (tokens[j].Type == TokenType.LParen) depth++;
            else if (tokens[j].Type == TokenType.RParen)
            {
                if (depth == 0)
                {
                    var inner = BuildExpressionTree(tokens, start + 1, j - 1);
                    pos = j + 1;
                    return inner;
                }
                depth--;
            }
            else if (tokens[j].Type == TokenType.Semicolon) throw new ParseException(TreeErrorType.UnexpectedSemicolon, tokens[j].Position);
        }
        throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
    }

    private Expr ParseLambdaExpr(List<Token> tokens, ref int pos, int end)
    {
        pos++; // skip λ
        var vars = new List<string>();
        int underscore = 0;
        while (pos <= end && tokens[pos].Type == TokenType.Term)
        {
            var v = tokens[pos].Value!;
            if (v == "_")
            {
                underscore++;
                v = $"_placeholder{underscore}";
            }
            vars.Add(v);
            pos++;
        }
        if (vars.Count == 0)
            throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[Math.Max(0, pos - 1)].Position);
        if (pos > end || tokens[pos].Type != TokenType.Dot)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[Math.Max(0, pos - 1)].Position);
        pos++; // skip '.'
        if (pos > end)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[Math.Max(0, pos - 1)].Position);
        var body = BuildExpressionTree(tokens, pos, end); // consume rest of span
        pos = end + 1;
        return vars.AsEnumerable().Reverse().Aggregate(body, (acc, v) => Expr.Abs(v, acc));
    }

    private Expr ParseLetExpr(List<Token> tokens, ref int pos, int end)
    {
        var letPos = tokens[pos].Position;
        pos++;
        bool isRec = pos <= end && tokens[pos].Type == TokenType.Rec;
        if (isRec) pos++;
        if (pos > end)
            throw new ParseException(TreeErrorType.MissingLetVariable, letPos);
        var names = new List<string>();
        var values = new List<Expr>();
        while (pos <= end)
        {
            if (tokens[pos].Type != TokenType.Term)
                throw new ParseException(TreeErrorType.MissingLetVariable, tokens[pos].Position);
            var name = tokens[pos].Value!;
            names.Add(name);
            pos++;
            if (pos > end || tokens[pos].Type != TokenType.Equals)
                throw new ParseException(TreeErrorType.MissingLetEquals, tokens[Math.Max(0, pos - 1)].Position);
            pos++; // skip '='
            // Detect arrow param list at the start (x, y -> body) or x, y -> body
            int arrowIdx = -1;
            TryGetArrowIndexForParamList(tokens, pos, end, out arrowIdx);
            // Find value end: stops at top-level 'in' or comma (with commas before arrow ignored)
            int valueStart = pos;
            int valueEnd = FindLetValueEnd(tokens, valueStart, end, arrowIdx);
            if (valueStart > valueEnd)
                throw new ParseException(TreeErrorType.MissingLetValue, tokens[Math.Max(0, valueStart)].Position);
            values.Add(BuildExpressionTree(tokens, valueStart, valueEnd));
            pos = valueEnd + 1;
            while (pos <= end && tokens[pos].Type == TokenType.Comma) pos++; // consume commas
            if (pos <= end && tokens[pos].Type == TokenType.In)
            {
                pos++;
                break;
            }
            if (pos <= end && tokens[pos].Type == TokenType.Term) continue; // next binding w/out comma
            if (pos > end) break;
            if (tokens[pos].Type != TokenType.Term) throw new ParseException(TreeErrorType.MissingLetIn, tokens[pos].Position);
        }
        if (pos > end) throw new ParseException(TreeErrorType.MissingLetBody, tokens[Math.Max(0, pos - 1)].Position);
        var bodyExpr = BuildExpressionTree(tokens, pos, end);
        pos = end + 1;
        if (isRec)
        {
            if (names.Count != 1) throw new ParseException(TreeErrorType.IllegalAssignment, letPos);
            var recVar = names[0];
            var recVal = values[0];
            var finalVal = Expr.App(Expr.YCombinator(), Expr.Abs(recVar, recVal));
            return Expr.App(Expr.Abs(recVar, bodyExpr), finalVal);
        }
        var abs = names.AsEnumerable().Reverse().Aggregate(bodyExpr, (b, n) => Expr.Abs(n, b));
        var app = abs;
        foreach (var v in values) app = Expr.App(app, v);
        return app;
    }

    private Expr CreateChurchNumeral(int n)
        => n < 1
        ? Expr.Abs("f", Expr.Abs("x", Expr.Var("x"))) 
        : Expr.Abs("f", Expr.Abs("x", GenerateApplicationChain("f", "x", n)));

    private Expr GenerateApplicationChain(string f, string x, int count)
        => Enumerable.Range(0, count).Aggregate(Expr.Var(x), (acc, _) => Expr.App(Expr.Var(f), acc));

    private Expr ParseListExpr(List<Token> tokens, ref int pos, int end)
    {
        int listStart = pos;
        pos++; // skip '['

        // Scan structure once: find closing ']' and first/second top-level occurrences
        ScanListStructure(tokens, pos, end, out int close, out int firstComma, out int commaCount, out int firstRange, out int rangeCount);

        var elements = new List<Expr>();

        // Stepped range: a , b .. c (exactly one comma and one '..' at top level, in order)
        if (firstComma != -1 && firstRange != -1 && firstRange > firstComma && commaCount == 1 && rangeCount == 1)
        {
            int aS = pos, aE = firstComma - 1;
            int bS = firstComma + 1, bE = firstRange - 1;
            int cS = firstRange + 1, cE = close - 1;
            if (aS <= aE && bS <= bE && cS <= cE)
            {
                bool ints = aS == aE && bS == bE && cS == cE
                            && tokens[aS].Type == TokenType.Integer
                            && tokens[bS].Type == TokenType.Integer
                            && tokens[cS].Type == TokenType.Integer;
                if (ints && int.TryParse(tokens[aS].Value, out int a)
                        && int.TryParse(tokens[bS].Value, out int b)
                        && int.TryParse(tokens[cS].Value, out int c))
                {
                    int step = b - a;
                    bool fwd = step > 0;
                    if (step == 0) elements.Add(CreateChurchNumeral(a));
                    else if (!(fwd && a > c) && !(!fwd && a < c))
                        for (int v = a; fwd ? v <= c : v >= c; v += step)
                            elements.Add(CreateChurchNumeral(v));
                    pos = close + 1;
                    return BuildList(elements);
                }
                var aExpr = BuildExpressionTree(tokens, aS, aE);
                var bExpr = BuildExpressionTree(tokens, bS, bE);
                var cExpr = BuildExpressionTree(tokens, cS, cE);
                var rangeSpec = new RangeSpec(aExpr, cExpr, bExpr, aS, cE);
                pos = close + 1;
                return Expr.App(Expr.App(Expr.App(Expr.Var("range2"), rangeSpec.Start), rangeSpec.Step!), rangeSpec.End);
            }
        }

        // Simple range: a .. b (no top-level commas, exactly one '..')
        if (firstRange != -1 && commaCount == 0 && rangeCount == 1)
        {
            int aS = pos, aE = firstRange - 1;
            int bS = firstRange + 1, bE = close - 1;
            if (aS <= aE && bS <= bE)
            {
                bool aInt = aS == aE && tokens[aS].Type == TokenType.Integer;
                bool bInt = bS == bE && tokens[bS].Type == TokenType.Integer;
                if (aInt && bInt && int.TryParse(tokens[aS].Value, out int from) && int.TryParse(tokens[bS].Value, out int to))
                {
                    if (from <= to) for (int v = from; v <= to; v++) elements.Add(CreateChurchNumeral(v));
                    else for (int v = from; v >= to; v--) elements.Add(CreateChurchNumeral(v));
                    pos = close + 1;
                    return BuildList(elements);
                }
                var aExpr = BuildExpressionTree(tokens, aS, aE);
                var bExpr = BuildExpressionTree(tokens, bS, bE);
                var rangeSpec = new RangeSpec(aExpr, bExpr, null, aS, bE);
                pos = close + 1;
                return Expr.App(Expr.App(Expr.Var("range"), rangeSpec.Start), rangeSpec.End);
            }
        }

        // General list: split on top-level commas up to closing bracket
        if (pos < close)
        {
            int segStart = pos, nest = 0;
            for (int i = pos; i < close; i++)
            {
                var tk = tokens[i];
                if (tk.Type is TokenType.LParen or TokenType.LBracket) nest++;
                else if (tk.Type is TokenType.RParen or TokenType.RBracket) nest--;
                else if (nest == 0 && tk.Type == TokenType.Comma)
                {
                    if (segStart < i)
                        elements.Add(BuildExpressionTree(tokens, segStart, i - 1));
                    segStart = i + 1;
                }
            }
            if (segStart < close)
                elements.Add(BuildExpressionTree(tokens, segStart, close - 1));
        }

        pos = close + 1;
        return BuildList(elements);

        // Local: Build cons-list using right-fold
        Expr BuildList(List<Expr> elems)
        {
            var res = Expr.Var("nil");
            for (int k = elems.Count - 1; k >= 0; k--)
                res = Expr.App(Expr.App(Expr.Var("cons"), elems[k]), res);
            return res;
        }
    }

    // Scan list structure between '[' (at start index - 1) and matching ']'
    private void ScanListStructure(List<Token> tokens, int start, int end, out int close, out int firstComma, out int commaCount, out int firstRange, out int rangeCount)
    {
        int nest = 0; close = -1; firstComma = -1; commaCount = 0; firstRange = -1; rangeCount = 0;
        for (int i = start; i <= end; i++)
        {
            var tk = tokens[i];
            if (tk.Type == TokenType.LParen || tk.Type == TokenType.LBracket) { nest++; continue; }
            if (tk.Type == TokenType.RParen) { nest--; continue; }
            if (tk.Type == TokenType.RBracket)
            {
                if (nest == 0) { close = i; break; }
                nest--; continue;
            }
            if (nest != 0) continue;
            if (tk.Type == TokenType.Comma)
            { if (firstComma == -1) firstComma = i; commaCount++; }
            else if (tk.Type == TokenType.Range)
            { if (firstRange == -1) firstRange = i; rangeCount++; }
        }
        if (close == -1) throw new ParseException(TreeErrorType.UnclosedParen, start < tokens.Count ? tokens[start].Position : 0);
    }

    // Detect if a parameter list starts at 'pos' and return the index of the following '->' if so.
    private static bool TryGetArrowIndexForParamList(List<Token> tokens, int pos, int end, out int arrowIdx)
    {
        arrowIdx = -1;
        if (pos > end) return false;
        if (tokens[pos].Type == TokenType.LParen)
        {
            int i = pos + 1;
            bool any = false;
            while (i <= end && tokens[i].Type == TokenType.Term)
            {
                any = true;
                i++;
                if (i <= end && tokens[i].Type == TokenType.Comma) { i++; continue; }
                break;
            }
            if (!any || i > end || tokens[i].Type != TokenType.RParen) return false;
            i++;
            if (i <= end && tokens[i].Type == TokenType.Arrow) { arrowIdx = i; return true; }
            return false;
        }
        if (tokens[pos].Type == TokenType.Term)
        {
            int j = pos;
            bool anyBare = false;
            while (j <= end && tokens[j].Type == TokenType.Term)
            {
                anyBare = true;
                j++;
                if (j <= end && tokens[j].Type == TokenType.Comma) { j++; continue; }
                break;
            }
            if (!anyBare) return false;
            if (j <= end && tokens[j].Type == TokenType.Arrow) { arrowIdx = j; return true; }
        }
        return false;
    }

    // Find the end index for a let-binding value, respecting nested let/in and param-list commas up to arrowIdx.
    private static int FindLetValueEnd(List<Token> tokens, int valueStart, int end, int arrowIdx)
    {
        int valueEnd = -1;
        int nesting = 0;
        int letNesting = 0;
        for (int i = valueStart; i <= end; i++)
        {
            var t = tokens[i];
            if (t.Type == TokenType.LParen || t.Type == TokenType.LBracket) { nesting++; continue; }
            if (t.Type == TokenType.RParen || t.Type == TokenType.RBracket) { nesting--; continue; }
            if (nesting != 0) continue;

            if (t.Type == TokenType.Let) { letNesting++; continue; }
            if (t.Type == TokenType.In)
            {
                if (letNesting > 0) { letNesting--; continue; }
                valueEnd = i - 1; break;
            }
            if (t.Type == TokenType.Comma && letNesting == 0)
            {
                // Ignore commas that are part of an initial param-list up to the arrow
                if (arrowIdx != -1 && i <= arrowIdx) continue;
                valueEnd = i - 1; break;
            }
        }
        if (valueEnd == -1) valueEnd = end;
        return valueEnd;
    }

    // Macro pass wrappers
    private MacroDefinition ParseMacroDefinition(List<Token> tokens) => _macroExpander.ParseMacroDefinition(tokens);
    private MacroPattern ParseMacroPattern(List<Token> tokens, ref int i) => _macroExpander.ParseMacroPattern(tokens, ref i);
    public Expr ExpandMacros(Expr expr) => _macroExpander.ExpandMacros(expr);
    public string DefineMacro(string name, IList<MacroPattern> pattern, Expr transformation) => _macroExpander.DefineMacro(name, pattern, transformation);
    public string ParseAndDefineMacro(string input) => _macroExpander.ParseAndDefineMacro(input);
    public List<string> ShowMacros() => _macroExpander.ShowMacros();
}
