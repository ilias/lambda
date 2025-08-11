// NOTE: This file has been refactored for readability. Tokenization & macro expansion
// have been moved into helper classes (Tokenizer, MacroExpander). Public API is preserved.
namespace LambdaCalculus;

// ---------------------------- Lexer / Tokens / Errors ----------------------------
public enum TokenType : byte { LParen, RParen, Lambda, Term, Equals, Integer, LBracket, RBracket, Comma, Dot, Y, Let, In, Rec, InfixOp, Arrow, Range, Macro, FatArrow, Dollar, Semicolon, Ellipsis }
public record Token(TokenType Type, int Position, string? Value = null);
public enum TreeErrorType : byte { UnclosedParen, UnopenedParen, MissingLambdaVar, MissingLambdaBody, EmptyExprList, IllegalAssignment, MissingLetVariable, MissingLetEquals, MissingLetIn, MissingLetValue, MissingLetBody, UnexpectedSemicolon, UnexpectedLambda, UnexpectedDot }
public class ParseException(TreeErrorType errorType, int position) : Exception($"{errorType} at position {position}")
{ public TreeErrorType ErrorType { get; } = errorType; public int Position { get; } = position; }

// ---------------------------- Statements ----------------------------
public enum StatementType : byte { Expr, Assignment }
public record Statement(StatementType Type, Expr Expression, string? VarName = null)
{ public static Statement ExprStatement(Expr expr) => new(StatementType.Expr, expr); public static Statement AssignmentStatement(string varName, Expr expr) => new(StatementType.Assignment, expr, varName); public override string ToString() => Type == StatementType.Expr ? Expression.ToString() : $"{VarName} = {Expression}"; }

// ---------------------------- Infix Operator Model ----------------------------
public enum Associativity : byte { Left, Right }
public record InfixOperator(string Symbol, int Precedence, Associativity Associativity, string? FunctionName = null)
{ public string GetFunctionName() => FunctionName ?? Symbol; }

// ---------------------------- Parser (Core) ----------------------------
public class Parser
{
    // Public (legacy) fields referenced by Interpreter
    public readonly Dictionary<string, InfixOperator> _infixOperators = new(StringComparer.Ordinal);
    public readonly Dictionary<string, List<MacroDefinition>> _macros = new(StringComparer.Ordinal);

    // Helper components
    private readonly Tokenizer _tokenizer;
    private readonly MacroExpander _macroExpander;

    public Parser()
    {
        DefineInfixOperator("|>", 1, "left"); // pipeline
        DefineInfixOperator(".", 9, "right"); // composition
        _tokenizer = new Tokenizer(this);
        _macroExpander = new MacroExpander(this);
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
    { "Y" => TokenType.Y, "let" => TokenType.Let, "in" => TokenType.In, "rec" => TokenType.Rec, ":macro" => TokenType.Macro, _ when term != null && IsInfixOperator(term) => TokenType.InfixOp, _ => TokenType.Term };

    // Delegated public API ------------------------------------------------------
    public List<Token> Tokenize(string input) => _tokenizer.Tokenize(input);

    // Backward compatible single statement parse
    public Statement? Parse(string input) => ParseAll(input).LastOrDefault();
    // Parse zero or more statements separated by top-level ';'
    public List<Statement> ParseAll(string input)
    {
        var tokens = Tokenize(input);
        if (tokens.Count == 0) return [];

        // Split by top-level semicolons (not inside parens/brackets)
        var segments = new List<(int start, int end)>();
        int parenDepth = 0, bracketDepth = 0;
        int segStart = 0;
        for (int i = 0; i < tokens.Count; i++)
        {
            var t = tokens[i];
            switch (t.Type)
            {
                case TokenType.LParen: parenDepth++; break;
                case TokenType.RParen: parenDepth--; break;
                case TokenType.LBracket: bracketDepth++; break;
                case TokenType.RBracket: bracketDepth--; break;
                case TokenType.Semicolon when parenDepth == 0 && bracketDepth == 0:
                    if (i - 1 >= segStart)
                        segments.Add((segStart, i - 1));
                    segStart = i + 1;
                    break;
            }
        }
        if (segStart < tokens.Count)
            segments.Add((segStart, tokens.Count - 1));

        var statements = new List<Statement>();
        foreach (var (start, end) in segments)
        {
            if (end < start) continue; // empty
            var slice = tokens.GetRange(start, end - start + 1);
            if (slice.Count == 0) continue;

            // Macro definition
            if (slice[0].Type == TokenType.Macro)
            { _macroExpander.ParseAndStoreMacroDefinition(slice); continue; }
            // Assignment
            if (slice.Count > 2 && slice[0].Type == TokenType.Term && slice[1].Type == TokenType.Equals)
            {
                var assignmentExpr = BuildExpressionTree(slice, 2, slice.Count - 1);
                var expandedAssignmentExpr = ExpandMacros(assignmentExpr);
                statements.Add(Statement.AssignmentStatement(slice[0].Value!, expandedAssignmentExpr));
                continue;
            }
            // Let expression
            if (slice[0].Type == TokenType.Let)
            {
                var idx = 0;
                var letExpr = ParseLetExpr(slice, ref idx, slice.Count - 1);
                statements.Add(Statement.ExprStatement(letExpr));
                continue;
            }
            // Plain expression
            var expr = BuildExpressionTree(slice, 0, slice.Count - 1);
            var expandedExpr = ExpandMacros(expr);
            statements.Add(Statement.ExprStatement(expandedExpr));
        }
        return statements;
    }
    internal Expr BuildExpressionTree(List<Token> tokens, int start, int end)
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
            var expr = token.Type switch
            {
                TokenType.LParen => ParseParenthesizedExpr(tokens, ref i, end),
                TokenType.RParen => throw new ParseException(TreeErrorType.UnopenedParen, token.Position),
                TokenType.LBracket => ParseListExpr(tokens, ref i, end),
                TokenType.RBracket => throw new ParseException(TreeErrorType.UnopenedParen, token.Position),
                TokenType.Semicolon => null, // ignore
                TokenType.Lambda => ParseLambdaExpr(tokens, ref i, end),
                TokenType.Let => ParseLetExpr(tokens, ref i, end),
                TokenType.Y => Expr.YCombinator(),
                TokenType.Term => Expr.Var(token.Value!),
                TokenType.Integer when int.TryParse(token.Value, out int value) => CreateChurchNumeral(value),
                TokenType.InfixOp => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position),
                TokenType.Arrow => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position),
                TokenType.Equals => throw new ParseException(TreeErrorType.IllegalAssignment, token.Position),
                _ => throw new ParseException(TreeErrorType.EmptyExprList, token.Position)
            };
            if (expr != null) expressions.Add(expr);
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
            else if (tokens[j].Type == TokenType.Semicolon)
                throw new ParseException(TreeErrorType.UnexpectedSemicolon, tokens[j].Position);
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
            if (v == "_") { underscoreCount++; v = $"_placeholder{underscoreCount}"; }
            variables.Add(v);
            i++;
        }
        if (variables.Count == 0)
            throw new ParseException(TreeErrorType.MissingLambdaVar, tokens[i - 1].Position);
        if (i > end)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i - 1].Position);
        if (tokens[i].Type == TokenType.Lambda)
            throw new ParseException(TreeErrorType.UnexpectedLambda, tokens[i].Position);
        if (tokens[i].Type != TokenType.Dot)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i - 1].Position);
        i++; // Skip dot
        if (i > end)
            throw new ParseException(TreeErrorType.MissingLambdaBody, tokens[i - 1].Position);
        if (tokens[i].Type == TokenType.Dot || (tokens[i].Type == TokenType.InfixOp && tokens[i].Value == "."))
            throw new ParseException(TreeErrorType.UnexpectedDot, tokens[i].Position);
        var lambdaBody = BuildExpressionTree(tokens, i, end);
        i = end;
        return variables.AsEnumerable().Reverse().Aggregate(lambdaBody, (body, var) => Expr.Abs(var, body));
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
        // Detect stepped range: [start , next .. end] with no additional top-level commas
        if (i <= end)
        {
            int scan = i;
            int nesting = 0;
            int firstComma = -1;
            int rangeIndex = -1;
            bool extraComma = false;
            while (scan <= end && tokens[scan].Type != TokenType.RBracket)
            {
                var tk = tokens[scan];
                if (tk.Type is TokenType.LParen or TokenType.LBracket) nesting++;
                else if (tk.Type is TokenType.RParen or TokenType.RBracket) nesting--;
                else if (nesting == 0)
                {
                    if (tk.Type == TokenType.Comma)
                    {
                        if (firstComma == -1) firstComma = scan; else { extraComma = true; break; }
                    }
                    else if (tk.Type == TokenType.Range)
                    {
                        if (rangeIndex == -1) rangeIndex = scan; else { rangeIndex = -2; break; }
                    }
                }
                scan++;
            }
            if (!extraComma && firstComma != -1 && rangeIndex > firstComma && rangeIndex != -2)
            {
                // Ensure no commas after range token (pure pattern)
                for (int k = rangeIndex + 1, nest2 = 0; k < scan && tokens[k].Type != TokenType.RBracket; k++)
                {
                    var tk2 = tokens[k];
                    if (tk2.Type is TokenType.LParen or TokenType.LBracket) nest2++;
                    else if (tk2.Type is TokenType.RParen or TokenType.RBracket) nest2--;
                    else if (nest2 == 0 && tk2.Type == TokenType.Comma) { extraComma = true; break; }
                }
                if (!extraComma)
                {
                    int startExprStart = i;
                    int startExprEnd = firstComma - 1;
                    int nextExprStart = firstComma + 1;
                    int nextExprEnd = rangeIndex - 1;
                    int endExprStart = rangeIndex + 1;
                    int endExprEnd = scan - 1;
                    if (startExprStart <= startExprEnd && nextExprStart <= nextExprEnd && endExprStart <= endExprEnd)
                    {
                        bool allInts = (startExprStart == startExprEnd && nextExprStart == nextExprEnd && endExprStart == endExprEnd &&
                            tokens[startExprStart].Type == TokenType.Integer && tokens[nextExprStart].Type == TokenType.Integer && tokens[endExprStart].Type == TokenType.Integer);
                        if (allInts && int.TryParse(tokens[startExprStart].Value, out int a) && int.TryParse(tokens[nextExprStart].Value, out int b) && int.TryParse(tokens[endExprStart].Value, out int c))
                        {
                            int step = b - a;
                            // Haskell-style semantics: step = b-a; produce sequence a, a+step, ... while within bound
                            if (step == 0)
                            {
                                elements.Add(CreateChurchNumeral(a));
                            }
                            else
                            {
                                bool forward = step > 0;
                                if ((forward && a > c) || (!forward && a < c))
                                {
                                    // Empty range per semantics – return []
                                }
                                else
                                {
                                    for (int v = a; forward ? v <= c : v >= c; v += step)
                                        elements.Add(CreateChurchNumeral(v));
                                }
                            }
                            // build list and return
                            var resList = Expr.Var("nil");
                            for (int m = elements.Count - 1; m >= 0; m--)
                                resList = Expr.App(Expr.App(Expr.Var("cons"), elements[m]), resList);
                            // advance to closing bracket
                            while (scan <= end && tokens[scan].Type != TokenType.RBracket) scan++;
                            if (scan > end || tokens[scan].Type != TokenType.RBracket)
                                throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
                            i = scan; // closing bracket
                            return resList;
                        }
                        // General case: desugar to (range2 start next end)
                        var startExpr = BuildExpressionTree(tokens, startExprStart, startExprEnd);
                        var nextExpr = BuildExpressionTree(tokens, nextExprStart, nextExprEnd);
                        var endExpr = BuildExpressionTree(tokens, endExprStart, endExprEnd);
                        var range2Call = Expr.App(Expr.App(Expr.App(Expr.Var("range2"), startExpr), nextExpr), endExpr);
                        while (scan <= end && tokens[scan].Type != TokenType.RBracket) scan++;
                        if (scan > end || tokens[scan].Type != TokenType.RBracket)
                            throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
                        i = scan;
                        return range2Call;
                    }
                }
            }
        }
        // Detect whole-list range form: [ expr .. expr ] where expr may be any expression (no top-level commas)
        if (i <= end)
        {
            int scan = i;
            int nesting = 0;
            int rangeIndex = -1;
            bool topLevelComma = false;
            while (scan <= end && tokens[scan].Type != TokenType.RBracket)
            {
                var tk = tokens[scan];
                if (tk.Type is TokenType.LParen or TokenType.LBracket) nesting++;
                else if (tk.Type is TokenType.RParen or TokenType.RBracket) nesting--;
                else if (nesting == 0)
                {
                    if (tk.Type == TokenType.Comma) { topLevelComma = true; break; }
                    if (tk.Type == TokenType.Range)
                    {
                        if (rangeIndex != -1) { rangeIndex = -2; break; } // multiple ranges
                        rangeIndex = scan;
                    }
                }
                scan++;
            }
            // If exactly one range and no commas, treat specially
            if (!topLevelComma && rangeIndex >= i && rangeIndex != -2)
            {
                // Extract start expression tokens
                int startExprStart = i;
                int startExprEnd = rangeIndex - 1;
                int endExprStart = rangeIndex + 1;
                int endExprEnd = scan - 1; // token before RBracket
                if (startExprStart <= startExprEnd && endExprStart <= endExprEnd)
                {
                    // Fast path: both endpoints are single integer tokens -> expand immediately (retain old behavior)
                    bool startIsInt = (startExprStart == startExprEnd && tokens[startExprStart].Type == TokenType.Integer);
                    bool endIsInt = (endExprStart == endExprEnd && tokens[endExprStart].Type == TokenType.Integer);
                    if (startIsInt && endIsInt && int.TryParse(tokens[startExprStart].Value, out int from) && int.TryParse(tokens[endExprStart].Value, out int to))
                    {
                        if (from <= to)
                            for (int v = from; v <= to; v++) elements.Add(CreateChurchNumeral(v));
                        else
                            for (int v = from; v >= to; v--) elements.Add(CreateChurchNumeral(v));
                        i = scan; // position at RBracket
                        // Build list from generated elements
                        var resultLit = Expr.Var("nil");
                        for (var j2 = elements.Count - 1; j2 >= 0; j2--)
                            resultLit = Expr.App(Expr.App(Expr.Var("cons"), elements[j2]), resultLit);
                        return resultLit;
                    }
                    // General case: desugar to (range start end)
                    var startExpr = BuildExpressionTree(tokens, startExprStart, startExprEnd);
                    var endExpr2 = BuildExpressionTree(tokens, endExprStart, endExprEnd);
                    // Build: ((range start) end)
                    var rangeCall = Expr.App(Expr.App(Expr.Var("range"), startExpr), endExpr2);
                    // Advance i to closing bracket
                    while (scan <= end && tokens[scan].Type != TokenType.RBracket) scan++;
                    if (scan > end || tokens[scan].Type != TokenType.RBracket)
                        throw new ParseException(TreeErrorType.UnclosedParen, tokens[start].Position);
                    i = scan; // set to RBracket
                    return rangeCall;
                }
            }
        }

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
    // Macro definition parsing delegated
    private MacroDefinition ParseMacroDefinition(List<Token> tokens) => _macroExpander.ParseMacroDefinition(tokens);
    
    private MacroPattern ParseMacroPattern(List<Token> tokens, ref int i) => _macroExpander.ParseMacroPattern(tokens, ref i);
    
    public Expr ExpandMacros(Expr expr) => _macroExpander.ExpandMacros(expr);
    
    // Macro API wrappers
    public string DefineMacro(string name, IList<MacroPattern> pattern, Expr transformation) => _macroExpander.DefineMacro(name, pattern, transformation);
    public string ParseAndDefineMacro(string input) => _macroExpander.ParseAndDefineMacro(input);
    public List<string> ShowMacros() => _macroExpander.ShowMacros();
}
