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
                var lit = t.Value!; i++;
                if (lit == "_") return MacroPattern.Wildcard();
                return MacroPattern.Literal(lit);
            case TokenType.Integer:
                // Support integer literal patterns (match only exact numeral occurrences)
                if (!int.TryParse(t.Value, out var intVal)) throw new ParseException(TreeErrorType.IllegalAssignment, t.Position);
                i++;
                return MacroPattern.IntLiteral(intVal);
            default: throw new ParseException(TreeErrorType.IllegalAssignment, t.Position);
        }
    }

    private static void ValidateRest(List<MacroPattern> patterns, List<Token> tokens, int pos)
    { var idx = patterns.FindIndex(p => p is VariablePattern vp && vp.IsRest); if (idx >= 0 && idx != patterns.Count -1) throw new ParseException(TreeErrorType.IllegalAssignment, tokens[pos].Position); }

    List<(string name, int depth, bool isOutput, string line)> macroUsages = [];
    public Expr ExpandMacros(Expr expr) {
        macroUsages.Clear();
        var result = ExpandRecursive(expr, 0);
        if (macroUsages.Count > 0)
        {
            var maxDepth = macroUsages.Max(p => p.depth);
            var maxLength = macroUsages.Max(p => p.name.Length);
            string nameFormat = $"{{0,-{maxLength}}}";
            foreach (var (name, depth, isOutput, line) in macroUsages)
            {
                var macroName = string.Format(nameFormat, isOutput ? "" : name);
                var namePadding = new string('.', depth);
                var depthPadding = new string(' ', maxDepth - depth);
                _logger?.Log($"Macro {depth,2} {namePadding}{macroName}{depthPadding} {line}");
            }
        }
        return result;
    }

    private Expr ExpandRecursive(Expr expr, int depth)
    {
        if (depth > 100) return expr;
        foreach (var entry in _parser._macros)
        {
            // Order macros by a computed specificity score so structural / literal-rich patterns beat generic variable catch-alls.
            var ordered = entry.Value
                .Select((m, idx) => (m, idx, score: ComputeSpecificityScore(m)))
                .OrderByDescending(t => t.score)          // most specific first
                .ThenByDescending(t => t.m.Pattern.Count) // then by arity (more patterns first)
                .ThenByDescending(t => t.idx)             // finally by recency (last defined wins ties)
                .Select(t => t.m);
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

    private static int ComputeSpecificityScore(MacroDefinition macro)
    {
        // Sum of pattern specificities; higher means more constrained.
        int total = 0;
        foreach (var p in macro.Pattern) total += PatternSpecificity(p);
        return total;
    }

    private static int PatternSpecificity(MacroPattern pattern) => pattern switch
    {
        LiteralPattern => 10,              // exact literal match
        IntLiteralPattern => 10,           // exact integer literal (if used)
        WildcardPattern => 0,              // matches anything, gives no specificity
        VariablePattern v => v.IsRest ? -1 : 0, // rest capture is very generic; slight penalty
        ListPattern lp => 3 + lp.Patterns.Sum(PatternSpecificity), // structural application spine
        _ => 0
    };

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
            macroUsages.Add((macro.Name, depth, false, $"in   {_interpreter!.FormatWithNumerals(expr)}"));
            macroUsages.Add((macro.Name, depth, true, $"out  {_interpreter!.FormatWithNumerals(expanded)}"));
            _interpreter._stats.MacroUsage[macro.Name] = _interpreter!._stats.MacroUsage.GetValueOrDefault(macro.Name) + 1;
            var recur = ExpandRecursive(expanded, depth + 1);
            // Log macro expansion: before, after (expanded), and final (after all macro expansion)
            macroUsages.Add((macro.Name, depth, false, $"done {_interpreter!.FormatWithNumerals(recur)}"));
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

    private static bool MatchSingle(Expr expr, MacroPattern pattern, Dictionary<string, Expr> bindings)
    {
        switch (pattern)
        {
            case LiteralPattern lit:
                return expr.Type == ExprType.Var && expr.VarName == lit.Value;
            case IntLiteralPattern intLit:
                // Numeral is Church encoded λf.λx.f(f(...x)) – detect by counting applications when pretty-printed? Simplest: re-normalize to numeral using interpreter's detection not available here.
                // We'll structural match against the canonical Church numeral form produced by parser (λf.λx. f^n x)
                return IsChurchNumeral(expr, intLit.Value);
            case VariablePattern v when !v.IsRest:
                bindings[v.Name] = expr; return true;
            case WildcardPattern:
                return true;
            case ListPattern listPattern:
                // Interpret listPattern.Patterns as an application spine pattern: (f a b) ≡ ((f a) b)
                return MatchApplicationSpine(expr, listPattern.Patterns, bindings);
            default:
                return false;
        }
    }

    private static bool IsChurchNumeral(Expr expr, int n)
    {
        // Expect λf.λx. f applied n times to x.
        if (expr.Type != ExprType.Abs) return false;
        var fVar = expr.AbsVarName!;
        var body = expr.AbsBody!;
        if (body.Type != ExprType.Abs) return false;
        var xVar = body.AbsVarName!;
        var inner = body.AbsBody!;
        // Unroll applications: should be f (f (... (f x))) n times, or just x when n==0
        int count = 0;
        while (inner.Type == ExprType.App)
        {
            var left = inner.AppLeft!;
            var right = inner.AppRight!;
            if (left.Type == ExprType.Var && left.VarName == fVar)
            {
                count++;
                inner = right;
            }
            else return false;
        }
        // Final must be xVar
        if (!(inner.Type == ExprType.Var && inner.VarName == xVar)) return false;
        return count == n;
    }

    private static bool MatchApplicationSpine(Expr expr, IList<MacroPattern> patterns, Dictionary<string, Expr> bindings)
    {
        if (patterns.Count == 0) return false;
        // Decompose expr into spine (left-assoc applications): collect from head outward.
        var nodes = new List<Expr>();
        var cur = expr;
        while (cur.Type == ExprType.App)
        {
            nodes.Insert(0, cur.AppRight!);
            cur = cur.AppLeft!;
        }
        nodes.Insert(0, cur); // head variable or abstraction
        if (nodes.Count != patterns.Count) return false;
        for (int k = 0; k < patterns.Count; k++)
        {
            if (!MatchSingle(nodes[k], patterns[k], bindings)) return false;
        }
        return true;
    }

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
