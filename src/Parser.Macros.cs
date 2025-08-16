using System.Collections.Generic;
using System.Linq;
namespace LambdaCalculus;

// Encapsulates macro parsing & expansion previously inside Parser.
internal sealed class MacroExpander
{
    private readonly Parser _parser;
    private readonly Logger? _logger;
    private readonly Interpreter? _interpreter;
    public MacroExpander(Parser parser, Logger? logger = null, Interpreter? interpreter = null)
    {
        _parser = parser;
        _logger = logger;
        _interpreter = interpreter;
    }

    public void ParseAndStoreMacroDefinition(List<Token> slice)
    {
        var macro = ParseMacroDefinition(slice);
        if (!_parser._macros.TryGetValue(macro.Name, out var list)) { list = []; _parser._macros[macro.Name] = list; }
        list.Add(macro);
    }

    public MacroDefinition ParseMacroDefinition(List<Token> tokens)
    {
        if (tokens.Count < 6 || tokens[0].Type != TokenType.Macro) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[0].Position);
        if (tokens[1].Type != TokenType.LParen) throw new ParseException(TreeErrorType.UnclosedParen, tokens[1].Position);
        if (tokens[2].Type != TokenType.Term) throw new ParseException(TreeErrorType.MissingLetVariable, tokens[2].Position);
        var name = tokens[2].Value!; var patterns = new List<MacroPattern>(); int i = 3;
        while (i < tokens.Count && tokens[i].Type != TokenType.RParen) patterns.Add(ParseMacroPattern(tokens, ref i));
        if (i >= tokens.Count || tokens[i].Type != TokenType.RParen) throw new ParseException(TreeErrorType.UnclosedParen, tokens[2].Position); i++;
        ValidateRest(patterns, tokens, i-1);
        Expr? guard = null;
        if (i < tokens.Count - 1 && tokens[i].Type == TokenType.Term && tokens[i].Value == "when")
        { i++; int gs = i; int ge = -1; for (int gi = i; gi < tokens.Count; gi++) if (tokens[gi].Type == TokenType.FatArrow) { ge = gi - 1; break; } if (ge < gs) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[i-1].Position); guard = ParseMacroGuardExpression(tokens, gs, ge); i = ge + 1; }
        if (i >= tokens.Count || tokens[i].Type != TokenType.FatArrow) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[Math.Max(i-1,0)].Position); i++;
        var transformation = ParseMacroTransformation(tokens, i, tokens.Count - 1);
        return new MacroDefinition(name, patterns, transformation, guard);
    }

    public MacroPattern ParseMacroPattern(List<Token> tokens, ref int i)
    {
        var t = tokens[i];
        switch (t.Type)
        {
            case TokenType.Dollar:
                i++; if (i >= tokens.Count || tokens[i].Type != TokenType.Term) throw new ParseException(TreeErrorType.MissingLetVariable, t.Position); var varName = tokens[i].Value!; i++; bool rest = false; if (i < tokens.Count && tokens[i].Type == TokenType.Ellipsis) { rest = true; i++; } return MacroPattern.Variable(varName, rest);
            case TokenType.LParen:
                var nested = new List<MacroPattern>(); i++; while (i < tokens.Count && tokens[i].Type != TokenType.RParen) nested.Add(ParseMacroPattern(tokens, ref i)); if (i >= tokens.Count || tokens[i].Type != TokenType.RParen) throw new ParseException(TreeErrorType.UnclosedParen, t.Position); i++; return MacroPattern.List(nested);
            case TokenType.Term:
                var lit = t.Value!; i++; return MacroPattern.Literal(lit);
            default: throw new ParseException(TreeErrorType.IllegalAssignment, t.Position);
        }
    }

    private static void ValidateRest(List<MacroPattern> patterns, List<Token> tokens, int pos)
    { var idx = patterns.FindIndex(p => p is VariablePattern vp && vp.IsRest); if (idx >= 0 && idx != patterns.Count -1) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[pos].Position); }

    public Expr ExpandMacros(Expr expr) => ExpandRecursive(expr, 0);

    private Expr ExpandRecursive(Expr expr, int depth)
    {
        if (depth > 100) return expr;
        foreach (var entry in _parser._macros)
        {
            var ordered = entry.Value.Select((m, idx) => (m, idx)).OrderByDescending(t => t.m.Pattern.Count).ThenByDescending(t => t.idx).Select(t => t.m);
            foreach (var macro in ordered)
            {
                var attempt = TryExpandMacro(expr, macro, depth);
                if (attempt.Success)
                {
                    return attempt.ExpandedExpr!;
                }
            }
        }
        return expr.Type switch
        {
            ExprType.Abs => Expr.Abs(expr.AbsVarName!, ExpandRecursive(expr.AbsBody!, depth)),
            ExprType.App => Expr.App(ExpandRecursive(expr.AppLeft!, depth), ExpandRecursive(expr.AppRight!, depth)),
            _ => expr
        };
    }

    private MacroExpansionResult TryExpandMacro(Expr expr, MacroDefinition macro, int depth)
    {
        var bindings = new Dictionary<string, Expr>();
        if (!MatchesStructure(expr, macro))
            return MacroExpansionResult.Failed("Structure mismatch");
        if (TryMatchPattern(expr, macro.Pattern, bindings))
        {
            if (macro.Guard is not null)
            {
                var guardEval = SubstituteMacroVariables(macro.Guard, bindings);
                if (guardEval.Type == ExprType.Var && guardEval.VarName == "false")
                    return MacroExpansionResult.Failed("Guard failed");
            }
            var expanded = SubstituteMacroVariables(macro.Transformation, bindings);
            // Log macro expansion
            _logger?.Log($"Macro {macro.Name} in:  {_interpreter!.FormatWithNumerals(expr)}");
            _logger?.Log($"Macro {macro.Name} out: {_interpreter!.FormatWithNumerals(expanded)}");
            var recur = ExpandRecursive(expanded, depth + 1);
            return MacroExpansionResult.Successful(recur);
        }
        return MacroExpansionResult.Failed("Pattern match failed");
    }

    private bool MatchesStructure(Expr expr, MacroDefinition macro)
    {
        var expected = macro.Pattern.Count; bool hasRest = macro.Pattern.Any(p => p is VariablePattern vp && vp.IsRest);
        if (expected == 0) return expr.Type == ExprType.Var && expr.VarName == macro.Name;
        var cur = expr; int argCount = 0; while (cur.Type == ExprType.App) { argCount++; cur = cur.AppLeft!; }
        return cur.Type == ExprType.Var && cur.VarName == macro.Name && (hasRest ? argCount >= expected - 1 : argCount == expected);
    }

    private bool TryMatchPattern(Expr expr, IList<MacroPattern> patterns, Dictionary<string, Expr> bindings)
    {
        if (patterns.Count == 0) return true; var cur = expr; var args = new List<Expr>();
        while (cur.Type == ExprType.App) { args.Insert(0, cur.AppRight!); cur = cur.AppLeft!; }
        int restIndex = -1; for (int k = 0; k < patterns.Count; k++) if (patterns[k] is VariablePattern vp && vp.IsRest) { restIndex = k; break; }
        if (restIndex >= 0)
        { if (args.Count < restIndex) return false; for (int k = 0; k < restIndex; k++) if (!MatchSingle(args[k], patterns[k], bindings)) return false; var restVar = (VariablePattern)patterns[restIndex]; var restArgs = args.Skip(restIndex).ToList(); Expr listExpr = Expr.Var("nil"); for (int j = restArgs.Count -1; j >=0; j--) listExpr = Expr.App(Expr.App(Expr.Var("cons"), restArgs[j]), listExpr); bindings[restVar.Name] = listExpr; return true; }
        if (args.Count != patterns.Count) return false; for (int k = 0; k < patterns.Count; k++) if (!MatchSingle(args[k], patterns[k], bindings)) return false; return true;
    }

    private static bool MatchSingle(Expr expr, MacroPattern pattern, Dictionary<string, Expr> bindings) => pattern switch
    {
        LiteralPattern lit => expr.Type == ExprType.Var && expr.VarName == lit.Value,
        VariablePattern v when !v.IsRest => (bindings[v.Name] = expr) == expr,
        ListPattern list => false, // TODO: Support structural matching for list patterns
        _ => false
    };

    private Expr SubstituteMacroVariables(Expr transformation, Dictionary<string, Expr> bindings) => transformation.Type switch
    { ExprType.Var when transformation.VarName != null && transformation.VarName.StartsWith("__MACRO_VAR_") => ExtractMacroVar(transformation.VarName, bindings) ?? transformation, ExprType.Var when transformation.VarName != null && transformation.VarName.StartsWith("__MACRO_INT_") => ConvertMacroInt(transformation.VarName), ExprType.Var when transformation.VarName != null && bindings.TryGetValue(transformation.VarName, out var val) => val, ExprType.Abs => SubstituteInLambda(transformation, bindings), ExprType.App => Expr.App(SubstituteMacroVariables(transformation.AppLeft!, bindings), SubstituteMacroVariables(transformation.AppRight!, bindings)), _ => transformation };

    private Expr SubstituteInLambda(Expr lambda, Dictionary<string, Expr> bindings)
    { var param = lambda.AbsVarName!; if (param.StartsWith("__MACRO_VAR_")) { var extracted = ExtractMacroVar(param, bindings); if (extracted?.Type == ExprType.Var && extracted.VarName != null) param = extracted.VarName; } return Expr.Abs(param, SubstituteMacroVariables(lambda.AbsBody!, bindings)); }

    private static Expr? ExtractMacroVar(string raw, Dictionary<string, Expr> bindings)
    { const string pfx = "__MACRO_VAR_"; if (raw.StartsWith(pfx)) { var name = raw[pfx.Length..]; if (bindings.TryGetValue(name, out var v)) return v; } return null; }

    private static Expr ConvertMacroInt(string raw)
    { const string pfx = "__MACRO_INT_"; if (raw.StartsWith(pfx) && int.TryParse(raw[pfx.Length..], out var n)) { // Church numeral
            var f = "f"; var x = "x"; Expr body = Expr.Var(x); for (int i = 0; i < n; i++) body = Expr.App(Expr.Var(f), body); return Expr.Abs(f, Expr.Abs(x, body)); } return Expr.Var(raw); }

    public string DefineMacro(string name, IList<MacroPattern> pattern, Expr transformation)
    { try { var m = new MacroDefinition(name, pattern, transformation); if (!_parser._macros.TryGetValue(name, out var list)) { list = []; _parser._macros[name] = list; } list.Add(m); return $"Macro '{name}' defined successfully"; } catch (Exception ex) { return $"Error defining macro '{name}': {ex.Message}"; } }

    public string ParseAndDefineMacro(string input)
    { try { var tokens = _parser.Tokenize(input); var macro = ParseMacroDefinitionFromInput(tokens); if (!_parser._macros.TryGetValue(macro.Name, out var list)) { list = []; _parser._macros[macro.Name] = list; } list.Add(macro); return $"Macro '{macro.Name}' defined successfully"; } catch (Exception ex) { return $"Error defining macro: {ex.Message}"; } }

    private MacroDefinition ParseMacroDefinitionFromInput(List<Token> tokens)
    { if (tokens.Count < 5) throw new ParseException(TreeErrorType.IllegalAssignment, 0); if (tokens[0].Type != TokenType.LParen) throw new ParseException(TreeErrorType.UnclosedParen, tokens[0].Position); if (tokens[1].Type != TokenType.Term) throw new ParseException(TreeErrorType.MissingLetVariable, tokens[1].Position); var name = tokens[1].Value!; var patterns = new List<MacroPattern>(); int i = 2; while (i < tokens.Count && tokens[i].Type != TokenType.RParen) patterns.Add(ParseMacroPattern(tokens, ref i)); if (i >= tokens.Count || tokens[i].Type != TokenType.RParen) throw new ParseException(TreeErrorType.UnclosedParen, tokens[1].Position); i++; ValidateRest(patterns, tokens, i-1); Expr? guard = null; if (i < tokens.Count - 1 && tokens[i].Type == TokenType.Term && tokens[i].Value == "when") { i++; int gs = i; int ge = -1; for (int gi = i; gi < tokens.Count; gi++) if (tokens[gi].Type == TokenType.FatArrow) { ge = gi - 1; break; } if (ge < gs) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[i-1].Position); guard = ParseMacroGuardExpression(tokens, gs, ge); i = ge + 1; } if (i >= tokens.Count || tokens[i].Type != TokenType.FatArrow) throw new ParseException(TreeErrorType.IllegalAssignment, i < tokens.Count ? tokens[i].Position : tokens[^1].Position); i++; var transformation = ParseMacroTransformation(tokens, i, tokens.Count - 1); return new MacroDefinition(name, patterns, transformation, guard); }

    private Expr ParseMacroTransformation(List<Token> tokens, int start, int end)
    { var processed = new List<Token>(); for (int i = start; i <= end; i++) { if (tokens[i].Type == TokenType.Lambda && i + 2 <= end && tokens[i+1].Type == TokenType.Dollar && tokens[i+2].Type == TokenType.Term) { var vn = tokens[i+2].Value!; processed.Add(tokens[i]); processed.Add(new Token(TokenType.Term, tokens[i+2].Position, $"__MACRO_VAR_{vn}")); if (i + 3 <= end && tokens[i+3].Type == TokenType.InfixOp && tokens[i+3].Value == ".") { processed.Add(new Token(TokenType.Dot, tokens[i+3].Position, ".")); i += 3; } else i += 2; }
            else if (tokens[i].Type == TokenType.Dollar && i + 1 <= end && tokens[i+1].Type == TokenType.Term) { var vn = tokens[i+1].Value!; processed.Add(new Token(TokenType.Term, tokens[i+1].Position, $"__MACRO_VAR_{vn}")); i++; }
            else if (tokens[i].Type == TokenType.Integer) { processed.Add(new Token(TokenType.Term, tokens[i].Position, $"__MACRO_INT_{tokens[i].Value}")); }
            else processed.Add(tokens[i]); }
        return _parser.BuildExpressionTree(processed, 0, processed.Count -1); }

    private Expr ParseMacroGuardExpression(List<Token> tokens, int start, int end)
    { var processed = new List<Token>(); for (int i = start; i <= end; i++) { if (tokens[i].Type == TokenType.Dollar && i + 1 <= end && tokens[i+1].Type == TokenType.Term) { var vn = tokens[i+1].Value!; processed.Add(new Token(TokenType.Term, tokens[i+1].Position, $"__MACRO_VAR_{vn}")); i++; } else if (tokens[i].Type == TokenType.Integer) { processed.Add(new Token(TokenType.Term, tokens[i].Position, $"__MACRO_INT_{tokens[i].Value}")); } else processed.Add(tokens[i]); } if (processed.Count == 0) throw new ParseException(TreeErrorType.EmptyExprList, start < tokens.Count ? tokens[start].Position : 0); return _parser.BuildExpressionTree(processed, 0, processed.Count -1); }

    public List<string> ShowMacros()
    { if (_parser._macros.Count == 0) return []; var res = new List<string>(); foreach (var (name, list) in _parser._macros.OrderBy(k => k.Key)) foreach (var m in list) res.Add($"  {m}"); return res; }
}
