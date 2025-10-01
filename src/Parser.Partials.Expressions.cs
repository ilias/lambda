namespace LambdaCalculus;

// Parser expression / atom related partial implementation extracted from monolithic Parser.cs
public partial class Parser
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

    /// <summary>
    /// Builds a left-nested application chain from a sequence of expressions.
    /// Example: [f, x, y] => ((f x) y)
    /// </summary>
    public static Expr BuildApplicationChain(IEnumerable<Expr> exprs)
    {
        var list = exprs as IList<Expr> ?? exprs.ToList();
        if (list.Count == 0) throw new ArgumentException("Empty application chain");
        var result = list[0];
        for (int i = 1; i < list.Count; i++)
            result = Expr.App(result, list[i]);
        return result;
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
    else if (IsArrowParamListAheadStrict(tokens, pos, end))
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

        // Lightweight syntactic sugar for ++ / -- both prefix and postfix if they appear
        // Patterns:
        //   ++ x   =>  succ x
        //   -- x   =>  pred x
        //   x ++   =>  succ x
        //   x --   =>  pred x
        // Only applied when exprs.Count == 2 to avoid interfering with longer chains.
        if (exprs.Count == 2)
        {
            bool firstIsOp = IsIncDecVar(exprs[0]);
            bool secondIsOp = IsIncDecVar(exprs[1]);
            if (firstIsOp && !secondIsOp)
            {
                var opName = exprs[0].VarName!;
                return Expr.App(Expr.Var(MapIncDec(opName)), exprs[1]);
            }
            if (!firstIsOp && secondIsOp)
            {
                var opName = exprs[1].VarName!;
                return Expr.App(Expr.Var(MapIncDec(opName)), exprs[0]);
            }
        }
        return BuildApplicationChain(exprs);
    }

    private static bool IsIncDecVar(Expr e) => e is { Type: ExprType.Var } ve && (ve.VarName == "++" || ve.VarName == "--");
    private static string MapIncDec(string name) => name == "++" ? "succ" : name == "--" ? "pred" : name;

    // Parse a single atomic / primary expression
    private Expr ParsePrimary(List<Token> tokens, ref int pos, int end)
    {
        if (pos > end)
            throw new ParseException(TreeErrorType.EmptyExprList, pos > 0 ? tokens[pos - 1].Position : 0);

        var token = tokens[pos];
        return token.Type switch
        {
            TokenType.QuasiQuote => ParseQuasiQuote(tokens, ref pos, end),
            TokenType.Unquote => ParseUnquote(tokens, ref pos, end),
            TokenType.UnquoteSplice => ParseUnquoteSplice(tokens, ref pos, end),
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
        => t is TokenType.LParen or TokenType.LBracket or TokenType.Lambda or TokenType.Term or TokenType.Integer or TokenType.Y or TokenType.Let or TokenType.QuasiQuote or TokenType.Unquote or TokenType.UnquoteSplice;

    private static bool IsArrowParamListAheadStrict(List<Token> tokens, int pos, int end)
    {
        if (pos > end) return false;
        // We only treat it as an arrow parameter list if an actual Arrow token appears before any non comma/non term/non paren token.
        // Exclude symbolic names like ++ / -- from being considered parameter identifiers.
        if (tokens[pos].Type == TokenType.Term && (tokens[pos].Value is "++" or "--")) return false;
        if (tokens[pos].Type == TokenType.LParen)
        {
            int i = pos + 1; bool sawName = false;
            while (i <= end && tokens[i].Type == TokenType.Term)
            {
                if (tokens[i].Value is "++" or "--") return false; // disallow symbolic tokens inside param list context
                sawName = true; i++;
                if (i <= end && tokens[i].Type == TokenType.Comma) { i++; continue; }
                break;
            }
            if (!sawName) return false;
            if (i > end || tokens[i].Type != TokenType.RParen) return false; i++;
            return i <= end && tokens[i].Type == TokenType.Arrow;
        }
        int j = pos; bool sawAny = false;
        while (j <= end && tokens[j].Type == TokenType.Term)
        {
            if (tokens[j].Value is "++" or "--") return false;
            sawAny = true; j++;
            if (j <= end && tokens[j].Type == TokenType.Comma) { j++; continue; }
            break;
        }
        if (!sawAny) return false;
        return j <= end && tokens[j].Type == TokenType.Arrow;
    }

    // ---------- Quasiquote / Unquote ----------
    private Expr ParseQuasiQuote(List<Token> tokens, ref int pos, int end)
    {
        // Consume qq or backtick token and quote the next expression (full expression, not only primary)
        pos++; // skip quasiquote
        if (pos > end) throw new ParseException(TreeErrorType.EmptyExprList, tokens[Math.Max(0, pos - 1)].Position);
        var quoted = ParseExpression(tokens, ref pos, end, 0);
        return Expr.Quote(quoted);
    }

    private Expr ParseUnquote(List<Token> tokens, ref int pos, int end)
    {
        // Unquote token must appear only within a quasiquote body; parser permits parse here, expander will enforce context.
        pos++; // skip '~'
        if (pos > end) throw new ParseException(TreeErrorType.EmptyExprList, tokens[Math.Max(0, pos - 1)].Position);
        // IMPORTANT: parse exactly one primary after ~ to keep it atomic in application/list spines.
        // Using ParseExpression here would allow ~(e) to accidentally absorb following primaries (e.g., arguments)
        // turning qq (f ~( (g x) ) y) into qq (f ~(g x y)), which is incorrect. Primary ensures a single unit.
        var inner = ParsePrimary(tokens, ref pos, end);
        return Expr.Unquote(inner);
    }

    private Expr ParseUnquoteSplice(List<Token> tokens, ref int pos, int end)
    {
        // Unquote-splicing token ~@, only meaningful inside list and application spines within quasiquote.
        pos++; // currently token already UnquoteSplice; move past it
        if (pos > end) throw new ParseException(TreeErrorType.EmptyExprList, tokens[Math.Max(0, pos - 1)].Position);
        // Similarly to Unquote, restrict to a single primary unit to avoid greedy absorption of following args/elements.
        var inner = ParsePrimary(tokens, ref pos, end);
        return Expr.UnquoteSplice(inner);
    }

    private Expr DesugarInfix(InfixOperator op, Expr left, Expr right)
    {
        if (op.Symbol == ".") return Expr.App(left, right); // original semantics: chaining application
        if (op.Symbol == "∘")
        {
            // Function composition: (f ∘ g)  ==>  λx. f (g x)
            var free = new HashSet<string>();
            CollectFreeVars(left, free);
            CollectFreeVars(right, free);
            string param = "x";
            if (free.Contains(param))
            {
                int i = 0;
                while (free.Contains(param = $"x{i}")) i++;
            }
            return Expr.Abs(param, Expr.App(left, Expr.App(right, Expr.Var(param))));
        }
        if (op.Symbol == "|>") return Expr.App(right, left);
    if (op.Symbol == "$") return Expr.App(left, right);
        var f = Expr.Var(op.GetFunctionName());
        return Expr.App(Expr.App(f, left), right);
    }

    // Minimal free variable collector (non-cached, parser-local) to avoid capture in composition desugaring.
    private static void CollectFreeVars(Expr expr, HashSet<string> free)
    {
        var bound = new HashSet<string>();
        var stack = new Stack<Expr>();
        stack.Push(expr);
        while (stack.Count > 0)
        {
            var cur = stack.Pop();
            switch (cur.Type)
            {
                case ExprType.Var:
                    if (cur.VarName != null && !bound.Contains(cur.VarName)) free.Add(cur.VarName);
                    break;
                case ExprType.Abs:
                    if (cur.AbsVarName != null)
                    {
                        bound.Add(cur.AbsVarName);
                        if (cur.AbsBody != null) stack.Push(cur.AbsBody);
                        // After exploring body, remove bound var (simulate scope exit) – use recursion simulation
                        // Here we can't easily know when scope ends with simple stack; accept slight over-approx:
                        // Over-approx only if shadowing occurs; safe for capture avoidance (may force new param name).
                    }
                    break;
                case ExprType.App:
                    if (cur.AppLeft != null) stack.Push(cur.AppLeft);
                    if (cur.AppRight != null) stack.Push(cur.AppRight);
                    break;
                case ExprType.Thunk:
                    if (cur.ThunkValue != null) stack.Push(cur.ThunkValue.Expression);
                    break;
            }
        }
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
            var rawValueExpr = BuildExpressionTree(tokens, valueStart, valueEnd);
            // Expand macros inside each let binding value before constructing the overall let abstraction
            rawValueExpr = ExpandMacros(rawValueExpr);
            values.Add(rawValueExpr);
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
    // Expand macros in let body prior to assembling lambda applications
    bodyExpr = ExpandMacros(bodyExpr);
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
        ScanGeneralList(tokens, pos, close, elements);

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

    private void ScanGeneralList(List<Token> tokens, int start, int close, List<Expr> elements)
    {
        if (start < close)
        {
            int segStart = start, nest = 0;
            for (int i = start; i < close; i++)
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
}
