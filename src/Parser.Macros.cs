using System.Collections.Generic;
using System.Linq;
namespace LambdaCalculus;

// Encapsulates macro parsing & expansion previously inside Parser.
internal sealed class MacroExpander
{
    private readonly Parser _parser;
    private readonly Logger? _logger;
    private readonly Interpreter? _interpreter;
    // Gensym counter for hygienic renaming of macro-introduced binders
    private int _gensymCounter = 0;
    private string Gensym(string hint) => $"__g_{hint}_{++_gensymCounter}";
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
            case TokenType.InfixOp when t.Value == "-":
                // Pattern form using separate '-' token before digits (e.g., (negSpec -2))
                if (i + 1 < tokens.Count && tokens[i+1].Type == TokenType.Integer && int.TryParse("-" + tokens[i+1].Value, out var negVal2))
                { i += 2; return MacroPattern.IntLiteral(negVal2); }
                throw new ParseException(TreeErrorType.IllegalAssignment, t.Position);
            case TokenType.Integer:
                // Support positive or negative integer literal patterns (token already includes sign if unary)
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
            ExprType.Quote => ExpandQuasiQuote(expr.QuoteBody!, depth),
            _ => expr
        };
    }

    // Expand a quoted template: walk it, replace Unquote nodes by their macro-expanded content, keep Quote context as plain structure
    private Expr ExpandQuasiQuote(Expr body, int depth)
    {
        return QQ(body, depth);

        Expr QQ(Expr node, int d)
        {
            switch (node.Type)
            {
                case ExprType.Unquote:
                    // Unquote: expand macros inside, then re-enter QQ to allow ~@ splicing
                    // in case the expanded expression is an application or list literal.
                    // This preserves expected semantics for templates like qq(~(f ~@ args)).
                    {
                        var expandedInner = ExpandRecursive(node.UnquoteBody!, d + 1);
                        return QQ(expandedInner, d);
                    }
                case ExprType.UnquoteSplice:
                    // Splice must be handled by caller context (application spine or list). Here, treat as-is marker.
                    // We'll signal back by returning the same node; wrapping contexts will detect and expand.
                    return Expr.UnquoteSplice(ExpandRecursive(node.UnquoteBody!, d + 1));
                case ExprType.Abs:
                    return Expr.Abs(node.AbsVarName!, QQ(node.AbsBody!, d));
                case ExprType.App:
                    // Special-case: cons-list construction to support splicing inside list literals
                    if (Expr.TryExtractListElements(node, out var rawElems))
                    {
                        var flat = new List<Expr>();
                        foreach (var el in rawElems)
                        {
                            var qel = QQ(el, d);
                            if (qel.Type == ExprType.UnquoteSplice)
                            {
                                var expanded = ExpandRecursive(qel.UnquoteBody!, d + 1);
                                if (Expr.TryExtractListElements(expanded, out var elems))
                                {
                                    flat.AddRange(elems);
                                }
                                else if (Expr.TryExtractChurchListElements(expanded, out var celems, Interpreter.ExtractChurchNumeralValue))
                                {
                                    flat.AddRange(celems);
                                }
                                else
                                {
                                    // Fallback: treat as single element
                                    flat.Add(expanded);
                                }
                            }
                            else if (qel.Type == ExprType.Unquote)
                            {
                                flat.Add(ExpandRecursive(qel.UnquoteBody!, d + 1));
                            }
                            else
                            {
                                flat.Add(qel);
                            }
                        }
                        // Rebuild cons-list from flattened elements
                        Expr list = Expr.Var("nil");
                        for (int k = flat.Count - 1; k >= 0; k--)
                            list = Expr.App(Expr.App(Expr.Var("cons"), flat[k]), list);
                        return list;
                    }
                    // Build application spine to support splicing of arguments
                    var spine = new List<Expr>();
                    var cur = node;
                    while (cur.Type == ExprType.App)
                    {
                        spine.Insert(0, cur.AppRight!);
                        cur = cur.AppLeft!;
                    }
                    spine.Insert(0, cur);
                    // First element is head; rest are arguments
                    var head = QQ(spine[0], d);
                    var args = new List<Expr>();
                    for (int i = 1; i < spine.Count; i++)
                    {
                        // Special handling: if the original node at this position is an Unquote, insert its expanded body as a single arg
                        // without re-walking via QQ (to avoid it accidentally absorbing following args). Splice (~@) still supported.
                        if (spine[i].Type == ExprType.Unquote)
                        {
                            args.Add(ExpandRecursive(spine[i].UnquoteBody!, d + 1));
                            continue;
                        }
                        if (spine[i].Type == ExprType.UnquoteSplice)
                        {
                            // Evaluate and splice a list of arguments; support church/cons lists
                            varseq(head, args, spine[i].UnquoteBody!, d);
                            continue;
                        }

                        var qarg = QQ(spine[i], d);
                        if (qarg.Type == ExprType.UnquoteSplice)
                        {
                            // Evaluate and splice a list of arguments; support church/cons lists
                            varseq(head, args, qarg.UnquoteBody!, d);
                        }
                        else if (qarg.Type == ExprType.Unquote)
                        {
                            // Defensive: if Unquote surfaced after QQ, expand its body and insert as single arg
                            args.Add(ExpandRecursive(qarg.UnquoteBody!, d + 1));
                        }
                        else
                        {
                            args.Add(qarg);
                        }
                    }
                    return RebuildApp(head, args);
                case ExprType.Thunk:
                    // Do not force thunks here; preserve structure
                    return Expr.Thunk(QQ(node.ThunkValue!.Expression, d), node.ThunkValue.Environment);
                case ExprType.Quote:
                    // Nested quote: flatten and continue (MVP: no depth semantics)
                    return QQ(node.QuoteBody!, d);
                default:
                    return node;
            }
        }

        // Rebuild application from head and args
        static Expr RebuildApp(Expr head, List<Expr> args)
        {
            var res = head;
            foreach (var a in args) res = Expr.App(res, a);
            return res;
        }

        void varseq(Expr head, List<Expr> args, Expr seqExpr, int d)
        {
            // Expand macros in the splice expression
            var expanded = ExpandRecursive(seqExpr, d + 1);
            // Try to extract a plain cons-list
            if (Expr.TryExtractListElements(expanded, out var elems))
            {
                args.AddRange(elems);
                return;
            }
            // Try to extract a Church-encoded list
            if (Expr.TryExtractChurchListElements(expanded, out var celems, Interpreter.ExtractChurchNumeralValue))
            {
                args.AddRange(celems);
                return;
            }
            // If it's nil, do nothing; otherwise, treat as single arg (fallback)
            args.Add(expanded);
        }
    }

    private static int ComputeSpecificityScore(MacroDefinition macro)
    {
        // Sum of pattern specificities; higher means more constrained.
        int total = 0;
        foreach (var p in macro.Pattern) total += PatternSpecificity(p);
        // Guard presence increases effective specificity vs unguarded generic clause.
        if (macro.Guard is not null) total += 1;
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
                // Substitute variables, expand macros within the guard, then evaluate it.
                var guardExpr = SubstituteMacroVariables(macro.Guard, bindings);
                guardExpr = ExpandRecursive(guardExpr, depth + 1); // allow nested macro usage inside guard
                // Evaluate guard expression to a value; treat Church false or literal false as failure.
                Expr evaluated;
                try { evaluated = _interpreter!.EvaluateCEK(guardExpr); }
                catch { return MacroExpansionResult.Failed("Guard evaluation error"); }
                // Recognize false via boolean extraction or literal var name.
                if ((evaluated.Type == ExprType.Var && evaluated.VarName == "false") || (Expr.TryExtractBoolean(evaluated, out var boolVal) && !boolVal))
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

    private bool MatchSingle(Expr expr, MacroPattern pattern, Dictionary<string, Expr> bindings)
    {
        switch (pattern)
        {
            case LiteralPattern lit:
                return expr.Type == ExprType.Var && expr.VarName == lit.Value;
            case IntLiteralPattern intLit:
                // First try direct structural match.
                if (IsChurchNumeral(expr, intLit.Value)) return true;
                // Attempt to evaluate expression (e.g., succ chains) to WHNF and test again (guard against divergence with try/catch).
                if (_interpreter != null)
                {
                    try
                    {
                        var evaluated = _interpreter.EvaluateCEK(expr);
                        if (IsChurchNumeral(evaluated, intLit.Value)) return true;
                    }
                    catch { /* ignore evaluation failures for matching */ }
                }
                return false;
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

    private bool MatchApplicationSpine(Expr expr, IList<MacroPattern> patterns, Dictionary<string, Expr> bindings)
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

    private Expr SubstituteMacroVariables(Expr transformation, Dictionary<string, Expr> bindings, string? renameOld = null, string? renameNew = null)
    {
        // Do not traverse inside quotes here; quotes/unquotes handled later by ExpandRecursive
        switch (transformation.Type)
        {
            case ExprType.Quote:
                // Process quoted templates so that placeholders inside Unquote/Splice are substituted;
                // also allow hygiene renaming for binders that the macro introduces inside the template.
                return Expr.Quote(SubstituteMacroVariables(transformation.QuoteBody!, bindings, renameOld, renameNew));

            case ExprType.Unquote:
                // Substitute placeholders inside the unquoted expression, but do NOT apply binder renaming
                // so that site code is not captured by macro renames.
                return Expr.Unquote(SubstituteMacroVariables(transformation.UnquoteBody!, bindings));

            case ExprType.UnquoteSplice:
                return Expr.UnquoteSplice(SubstituteMacroVariables(transformation.UnquoteBody!, bindings));

            case ExprType.Var:
                {
                    var name = transformation.VarName!;
                    // Macro placeholders: substitute and do not apply renames inside substituted tree
                    if (!string.IsNullOrEmpty(name) && name.StartsWith("__MACRO_VAR_"))
                        return ExtractMacroVar(name, bindings) ?? transformation;
                    if (!string.IsNullOrEmpty(name) && name.StartsWith("__MACRO_INT_"))
                        return ConvertMacroInt(name);
                    // Direct binding by plain name
                    if (!string.IsNullOrEmpty(name) && bindings.TryGetValue(name, out var val))
                        return val;
                    // Apply scoped literal rename if requested
                    if (renameOld is not null && renameNew is not null && name == renameOld)
                        return Expr.Var(renameNew);
                    return transformation;
                }

            case ExprType.Abs:
                {
                    var lambda = transformation;
                    var param = lambda.AbsVarName!;
                    // If parameter originates from a macro variable placeholder, extract a safe name
                    if (param.StartsWith("__MACRO_VAR_"))
                    {
                        var extracted = ExtractMacroVar(param, bindings);
                        string paramName;
                        if (extracted?.Type == ExprType.Var && extracted.VarName is not null)
                            paramName = extracted.VarName;
                        else
                            paramName = Gensym("p"); // Fallback to fresh if not a variable
                        var bodyProcessed = SubstituteMacroVariables(lambda.AbsBody!, bindings);
                        return Expr.Abs(paramName, bodyProcessed);
                    }

                    // Hygienic default: macro-introduced binder gets a fresh name
                    var fresh = Gensym(param);
                    var newBody = SubstituteMacroVariables(lambda.AbsBody!, bindings, renameOld: param, renameNew: fresh);
                    return Expr.Abs(fresh, newBody);
                }

            case ExprType.App:
                return Expr.App(
                    SubstituteMacroVariables(transformation.AppLeft!, bindings, renameOld, renameNew),
                    SubstituteMacroVariables(transformation.AppRight!, bindings, renameOld, renameNew));

            default:
                return transformation;
        }
    }

    private Expr SubstituteInLambda(Expr lambda, Dictionary<string, Expr> bindings)
    { var param = lambda.AbsVarName!; if (param.StartsWith("__MACRO_VAR_")) { var extracted = ExtractMacroVar(param, bindings); if (extracted?.Type == ExprType.Var && extracted.VarName != null) param = extracted.VarName; else param = Gensym("p"); return Expr.Abs(param, SubstituteMacroVariables(lambda.AbsBody!, bindings)); } var fresh = Gensym(param); return Expr.Abs(fresh, SubstituteMacroVariables(lambda.AbsBody!, bindings, param, fresh)); }

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
