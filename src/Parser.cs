/*
========================================
 Minimal Grammar (Single Source of Truth)
 Generated from parser rules below
----------------------------------------
 program        ::= expr (';' expr)*                      // ParseProgram
 expr           ::= letExpr | arrowExpr | infixExpr       // ParseExpression
 letExpr        ::= 'let' bindings 'in' expr              // ParseLetExpr
 bindings       ::= binding (',' binding)*                // ParseLetExpr (multi-binding)
 binding        ::= name '=' expr                         // ParseLetExpr (single binding)
 arrowExpr      ::= paramList '->' expr                   // ParseArrowExpr
 paramList      ::= name (',' name)* | '(' name (',' name)* ')' // IsArrowParamListAhead
 infixExpr      ::= appExpr (infixOp appExpr)*            // ParseApplication + Pratt
 appExpr        ::= primary (primary)*                    // ParseApplication
 primary        ::= integer | name | '(' expr ')' | listExpr // ParsePrimary
 listExpr       ::= '[' exprList ']'                      // ParseListExpr
 exprList       ::= expr (',' expr)*                      // ParseListExpr
 defFunction    ::= 'def' name name* '=' expr             // (syntactic sugar) ⇒ name = p1,p2 -> expr
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

public partial class Parser
{
    public readonly Dictionary<string, InfixOperator> _infixOperators = new(StringComparer.Ordinal);
    public readonly Dictionary<string, List<MacroDefinition>> _macros = new(StringComparer.Ordinal);
    private readonly Tokenizer _tokenizer;
    private readonly MacroExpander _macroExpander;
    private readonly Logger? _logger;

    public Parser()
    {
        _logger = null;
    DefineInfixOperator("|>", 1, "left");
    DefineInfixOperator("$", 1, "right");
    DefineInfixOperator(".", 9, "right"); // application chaining
    DefineInfixOperator("∘", 9, "right"); // true composition
        _tokenizer = new Tokenizer(this);
        _macroExpander = new MacroExpander(this, null);
    }

    public Parser(Logger logger, Interpreter interpreter)
    {
        _logger = logger;
    DefineInfixOperator("|>", 1, "left");
    DefineInfixOperator("$", 1, "right");
    DefineInfixOperator(".", 9, "right");
    DefineInfixOperator("∘", 9, "right");
        _tokenizer = new Tokenizer(this);
        _macroExpander = new MacroExpander(this, logger, interpreter);
    }

    // Expression / application parsing helpers moved to Parser.Partials.Expressions.cs

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
            // 'def' function definition sugar: def f x y = body  ==>  f = x,y -> body
            if (slice[0].Type == TokenType.Term && slice[0].Value == "def")
            {
                if (slice.Count < 4) // def name = expr  (or missing parts)
                {
                    // Fall through to normal handling if malformed; will likely raise parse error later
                }
                else
                {
                    // Find '=' token position
                    int eqIndex = slice.FindIndex(t => t.Type == TokenType.Equals);
                    if (eqIndex > 1 && eqIndex != -1)
                    {
                        var nameTok = slice[1];
                        if (nameTok.Type == TokenType.Term || nameTok.Type == TokenType.InfixOp)
                        {
                            var paramTokens = slice.Skip(2).Take(eqIndex - 2).Where(t => t.Type == TokenType.Term).ToList();
                            var bodyTokens = slice.Skip(eqIndex + 1).ToList();
                            if (bodyTokens.Count > 0)
                            {
                                var transformed = new List<Token>();
                                transformed.Add(new Token(nameTok.Type, nameTok.Position, nameTok.Value));
                                transformed.Add(new Token(TokenType.Equals, nameTok.Position, "="));
                                if (paramTokens.Count == 0)
                                {
                                    // Simple assignment: def f = body  ==> f = body
                                    transformed.AddRange(bodyTokens);
                                }
                                else
                                {
                                    // Build param list with commas, then arrow, then body
                                    for (int pi = 0; pi < paramTokens.Count; pi++)
                                    {
                                        transformed.Add(new Token(TokenType.Term, paramTokens[pi].Position, paramTokens[pi].Value));
                                        if (pi < paramTokens.Count - 1)
                                            transformed.Add(new Token(TokenType.Comma, paramTokens[pi].Position, ","));
                                    }
                                    transformed.Add(new Token(TokenType.Arrow, bodyTokens[0].Position, "->"));
                                    transformed.AddRange(bodyTokens);
                                }
                                // Re-parse transformed slice as if user wrote the desugared form
                                slice = transformed;
                            }
                        }
                    }
                }
            }
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

    // Macro pass wrappers
    private MacroDefinition ParseMacroDefinition(List<Token> tokens) => _macroExpander.ParseMacroDefinition(tokens);
    private MacroPattern ParseMacroPattern(List<Token> tokens, ref int i) => _macroExpander.ParseMacroPattern(tokens, ref i);
    public Expr ExpandMacros(Expr expr) => _macroExpander.ExpandMacros(expr);
    public string DefineMacro(string name, IList<MacroPattern> pattern, Expr transformation) => _macroExpander.DefineMacro(name, pattern, transformation);
    public string ParseAndDefineMacro(string input) => _macroExpander.ParseAndDefineMacro(input);
    public List<string> ShowMacros() => _macroExpander.ShowMacros();
}
