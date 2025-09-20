namespace LambdaCalculus;

public partial class Interpreter
{
    // --- Module system (namespacing) -------------------------------------------------------
    private sealed class ModuleInfo
    {
        public required string Alias { get; init; }
        public required string SourcePath { get; init; }
        public Dictionary<string, Expr> Env { get; } = new(StringComparer.Ordinal); // unqualified -> qualified expr
        public HashSet<string> QualifiedKeys { get; } = new(StringComparer.Ordinal);
        public DateTime LoadedAt { get; } = DateTime.UtcNow;
    }

    private readonly Dictionary<string, ModuleInfo> _modules = new(StringComparer.Ordinal);
    // Track module file paths currently being loaded to prevent cycles
    private readonly HashSet<string> _moduleLoadStack = new(StringComparer.OrdinalIgnoreCase);

    private readonly Dictionary<string, Expr> _context = new(StringComparer.Ordinal);
    private readonly Dictionary<string, Expr> _contextUnevaluated = new(StringComparer.Ordinal);
    private readonly Dictionary<SubstitutionCacheKey, Expr> _substitutionCache = new(8192);
    private readonly Dictionary<Expr, Expr> _evaluationCache = new(8192, new ExprEqualityComparer());
    private readonly Dictionary<Expr, HashSet<string>> _freeVarCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _expressionPool = new(2048, StringComparer.Ordinal);
    private readonly Dictionary<(Expr, string), bool> _containsVarCache = new(2048);
    private readonly Dictionary<Expr, Expr> _normalizationCache = new(4096, new ExprEqualityComparer());
    private readonly Dictionary<string, Expr> _variableCache = new(1024);
    private readonly Logger _logger;
    public readonly Statistics _stats;
    public readonly Parser _parser;
    private IEvaluator _evaluator; // strategy
    private readonly System.Diagnostics.Stopwatch _perfStopwatch = new();
    private bool _showStep = false;
    private bool _lazyEvaluation = true;
    private bool _prettyPrint = true;
    internal int _nativeArithmetic = 0; // internal for services
    private bool _useNativeArithmetic = true;
    private bool _usedRandom = false;
    internal EqualityService Equality { get; private set; } = null!;
    // Test output mode & records (for :test json/text)
    private enum TestOutputMode { Text, Json }
    private TestOutputMode _testOutputMode = TestOutputMode.Text;
    private readonly List<TestRecord> _testRecords = new();
    private sealed record TestRecord(string Kind, bool Passed, string Left, string Right, int CallIndex, double SuccessRate, string? Line, string? File, int? FileLine);
    internal string? _currentInputLine = null; // latest raw input segment being processed
    internal string? _currentSourceFile = null; // file path when loading, else null
    internal int? _currentSourceLine = null; // 1-based line number when loading
    private bool _isLoadingFile = false; // guard to keep file context during batch load
    // macroExpandedLine removed
    // REPL QoL: history & last result tracking
    private readonly List<string> _history = new(1024);
    private Expr? _lastResultExpr = null; // last successfully evaluated (non-assignment) expression value
    private const int MaxHistory = 10_000;
    internal string? _lastLoadedFile = null; // for :reload

    public Interpreter(Logger logger, Statistics? stats = null)
    {
        _logger = logger;
        _stats = stats ?? new Statistics();
        _parser = new Parser(logger, this); // Pass logger to parser
        _evaluator = new CEKEvaluator(this); // default strategy
        Equality = new EqualityService(this);
        RegisterNativeFunctions();
    }

    /// <summary>
    /// Public read-only access to the internal <see cref="Logger"/> instance. Exposed to allow
    /// hosting layers (CLI / Web) to adjust buffering or retrieve captured logs without reflection.
    /// </summary>
    public Logger Logger => _logger;
    internal Logger LoggerInstance => _logger; // service access

    // Expose selected helpers for service classes (kept minimal surface)

    /// <summary>
    /// Processes a raw REPL input line which may contain multiple semicolon-separated statements
    /// (commands, assignments, or expressions) and returns the last evaluated expression (if any)
    /// plus the aggregated textual output produced during processing.
    /// </summary>
    /// <param name="input">User supplied input line.</param>
    /// <returns>Tuple of the last resulting expression (or null) and a formatted output string.</returns>
    public async Task<(Expr? exp, string str)> ProcessInputAsync(string input)
    {
        try
        {
            _stats.Iterations = 0;
            _usedRandom = false;
            if (!_isLoadingFile)
            {
                _currentSourceFile = null; // interactive resets file context
                _currentSourceLine = null;
            }
            if (string.IsNullOrWhiteSpace(input)) return (null, "");
            input = input.TrimEnd('\\');
            // Comments (whole-line)
            if (input.StartsWith('#')) return (null, input.Trim());

            // Split input into top-level segments by ';' (outside parens/brackets)
            var segments = SplitTopLevelSegments(input);
            if (segments.Count == 0) return (null, "");

            // Fast path: single segment that is a command
            if (segments.Count == 1 && segments[0].TrimStart().StartsWith(':'))
                return (null, await HandleCommandAsync(segments[0]));

            var sb = new System.Text.StringBuilder();
            Expr? lastExpr = null;
            foreach (var seg in segments)
            {
                var trimmed = seg.Trim();
                if (trimmed.Length == 0) continue;
                _currentInputLine = trimmed; // capture segment for test recording
                if (trimmed.StartsWith('#')) continue; // skip comment segment
                // Record original top-level segment in history (commands added in fast path earlier)
                if (!trimmed.StartsWith(':'))
                {
                    if (_history.Count == 0 || _history[^1] != trimmed)
                    {
                        _history.Add(trimmed);
                        if (_history.Count > MaxHistory) _history.RemoveRange(0, _history.Count - MaxHistory);
                    }
                }

                if (trimmed.StartsWith(':'))
                {
                    // Command segment
                    var cmdResult = await HandleCommandAsync(trimmed);
                    sb.AppendLine(cmdResult);
                    // If exit/quit, stop processing further segments
                    if (trimmed is ":exit" or ":quit")
                        break;
                    continue;
                }

                // Expression / assignment segment (may itself contain multiple assignments if user grouped without semicolons)
                var statements = _parser.ParseAll(trimmed);
                foreach (var st in statements)
                {
                    if (st.Type == StatementType.Assignment)
                    {
                        _contextUnevaluated[st.VarName!] = st.Expression;
                        var val = _evaluator.Evaluate(st.Expression);
                        _context[st.VarName!] = val;
                        sb.AppendLine($"-> {st.VarName} = {FormatWithNumerals(val)}");
                        lastExpr = null;
                        _lastResultExpr = lastExpr;
                    }
                    else
                    {
                        _logger.Log($"Eval: {FormatWithNumerals(st.Expression)}");
                        if (_showStep) _logger.Log($"Processing: {st}");
                        _lastResultExpr = lastExpr;
                        var res = _evaluator.Evaluate(st.Expression);
                        var norm = NormalizeExpression(res);
                        _stats.TotalIterations += _stats.Iterations;
                        sb.AppendLine($"-> {FormatWithNumerals(norm)}");
                        lastExpr = norm;
                    }
                }
            }

            return (lastExpr, sb.ToString().TrimEnd());
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

    // Split a raw input line into top-level semicolon-separated segments, ignoring semicolons inside () and []
    private static List<string> SplitTopLevelSegments(string input)
    {
        var segments = new List<string>();
        int paren = 0, bracket = 0; int start = 0;
        for (int i = 0; i < input.Length; i++)
        {
            var c = input[i];
            switch (c)
            {
                case '(': paren++; break;
                case ')': if (paren > 0) paren--; break;
                case '[': bracket++; break;
                case ']': if (bracket > 0) bracket--; break;
                case ';' when paren == 0 && bracket == 0:
                    var seg = input.Substring(start, i - start).Trim();
                    if (seg.Length > 0) segments.Add(seg);
                    start = i + 1;
                    break;
            }
        }
        if (start < input.Length)
        {
            var last = input[start..].Trim();
            if (last.Length > 0) segments.Add(last);
        }
        return segments;
    }

    // Ensure built-in range and range2 functions exist (lazy, stepped ranges)
    /// <summary>
    /// Ensures built-in range producing functions (range / range2) are present. They are defined lazily
    /// only if not already in the environment so user overrides are respected.
    /// </summary>

    /// <summary>
    /// Emits the standard CLI-style output lines (Name / Time / result lines) into the logger buffer.
    /// Exposed so hosting layers (e.g., web UI) can reproduce identical output formatting.
    /// </summary>
    public async Task DisplayOutput((Expr? expr, string str) result, TimeSpan elapsed)
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

    private async Task<string> HandleCommandAsync(string input)
    {
        var parts = input.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
        var command = parts[0];
        var arg = parts.Length > 1 ? parts[1].Trim() : "";
        return command switch
        {
            ":test" when arg.Equals("clear", StringComparison.OrdinalIgnoreCase) => TestClear(),
            ":test" when arg.Equals("result", StringComparison.OrdinalIgnoreCase) => TestResult(),
            ":test" when arg.Equals("json", StringComparison.OrdinalIgnoreCase) => SetTestOutputMode(TestOutputMode.Json),
            ":test" when arg.Equals("text", StringComparison.OrdinalIgnoreCase) => SetTestOutputMode(TestOutputMode.Text),
            ":hist" => HandleHistory(arg),
            ":repeat" => await HandleRepeatAsync(arg),
            ":reload" => await HandleReloadAsync(),
            ":last" => HandleLast(),
            ":log" => await _logger.HandleLogCommandAsync(arg),
            ":load" => await LoadFileAsync(arg),
            ":save" => await SaveFileAsync(arg),
            ":step" => HandleStep(arg),
            ":lazy" => HandleLazy(arg),
            ":clear" => HandleClear(arg),
            ":stats" => ShowStats(),
            ":help" => ShowHelp(),
            ":env" or ":environment" => await ShowEnv(arg),
            ":exit" or ":quit" => "bye",
            ":depth" => HandleRecursionDepth(arg),
            ":infix" => HandleInfixCommand(arg),
            ":native" => HandleNativeArithmetic(arg),
            ":pretty" or ":pp"=> HandlePrettyPrint(arg),
            ":macro" => HandleMacroDefinition(arg),
            ":module" => await HandleModuleCommandAsync(arg),
            _ => $"Unknown command: {command}"
        };
    }

    // --- Module commands -------------------------------------------------------------------
    private async Task<string> HandleModuleCommandAsync(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg))
            return "Usage: :module <load|reload|unload|list|import|alias|with|clear-imports> ...";

        var parts = arg.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        var action = parts[0].ToLowerInvariant();
        switch (action)
        {
            case "list":
                if (_modules.Count == 0) return "# (no modules)";
                var lines = _modules
                    .OrderBy(kv => kv.Key)
                    .Select(kv => $"  {kv.Key} -> {kv.Value.SourcePath} ({kv.Value.Env.Count} symbols)");
                return string.Join('\n', lines);

            case "load":
                // :module load "path" as Alias
                return await ModuleLoadAsync(parts);

            case "reload":
                // :module reload Alias
                return await ModuleReloadAsync(parts);

            case "unload":
                // :module unload Alias
                return ModuleUnload(parts);

            case "alias":
                // :module alias OldAlias as NewAlias (rename reference only)
                return ModuleAlias(parts);

            case "import":
                // :module import Alias::{a,b,c} or Alias::{a as x, b}
                return ModuleImport(arg[("import".Length)..].Trim());

            case "with":
                // :module with Alias => <expr>
                return await ModuleWithAsync(arg[("with".Length)..].Trim());

            case "clear-imports":
                return ModuleClearImports();

            default:
                return $"Unknown :module action: {action}";
        }
    }

    private async Task<string> ModuleLoadAsync(string[] parts)
    {
        // Expect: load <path> as <Alias>
        // Handle quoted path
        var rest = string.Join(' ', parts.Skip(1));
        if (string.IsNullOrWhiteSpace(rest)) return "Usage: :module load \"file.lambda\" as Alias";
        string? path = null; string? alias = null;
        if (rest.StartsWith('"'))
        {
            int end = rest.IndexOf('"', 1);
            if (end <= 0) return "Error: Unterminated path string";
            path = rest.Substring(1, end - 1);
            var after = rest.Substring(end + 1).Trim();
            if (!after.StartsWith("as ", StringComparison.OrdinalIgnoreCase)) return "Error: Expected 'as Alias'";
            alias = after[3..].Trim();
        }
        else
        {
            var tokens = rest.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            if (tokens.Length < 3 || !string.Equals(tokens[^2], "as", StringComparison.OrdinalIgnoreCase))
                return "Usage: :module load <path> as Alias";
            path = string.Join(' ', tokens[..^2]);
            alias = tokens[^1];
        }
        if (string.IsNullOrWhiteSpace(path) || string.IsNullOrWhiteSpace(alias)) return "Error: Missing path or alias";

        // Resolve relative paths against the directory of the currently loading file when available
        string resolvedPath;
        try
        {
            var baseDir = _currentSourceFile is not null && File.Exists(_currentSourceFile)
                ? Path.GetDirectoryName(_currentSourceFile) ?? Directory.GetCurrentDirectory()
                : Directory.GetCurrentDirectory();
            resolvedPath = Path.IsPathRooted(path) ? path : Path.GetFullPath(Path.Combine(baseDir, path));
        }
        catch
        {
            resolvedPath = path; // fallback to raw
        }
        if (!File.Exists(resolvedPath)) return $"File not found: {path}";

        // Cycle guard by absolute path
        var abs = Path.GetFullPath(resolvedPath);
        if (_moduleLoadStack.Contains(abs))
            return $"Error: Detected cyclic module load involving '{resolvedPath}'";
        _moduleLoadStack.Add(abs);

        // Load into an isolated interpreter to capture its definitions, then rewrite qualified
        var temp = new Interpreter(new Logger(), new Statistics());
        var loadRes = await temp.LoadFileAsync(resolvedPath);
        _ = loadRes; // ignore text
        try
        {
            var mod = new ModuleInfo { Alias = alias, SourcePath = abs };
            var moduleNames = new HashSet<string>(temp._contextUnevaluated.Keys, StringComparer.Ordinal);

            // Copy only direct (unqualified) definitions from temp as the parent module's own members
            foreach (var kv in temp._contextUnevaluated.Where(kv => !kv.Key.Contains("::")))
            {
                var qualifiedName = Qualify(alias, kv.Key);
                var rewritten = RewriteQualifiedNames(kv.Value, alias, moduleNames);
                mod.Env[kv.Key] = rewritten;
                _context[qualifiedName] = _evaluator.Evaluate(rewritten);
                _contextUnevaluated[qualifiedName] = rewritten;
                mod.QualifiedKeys.Add(qualifiedName);
            }

            // Bring submodules declared inside the module file into this interpreter as hierarchical modules
            // Example: inside file, ":module load \"B.lambda\" as B" becomes host alias "Alias.B" and qual names "Alias::B::x"
            foreach (var child in temp._modules.OrderBy(k => k.Key))
            {
                var childAliasPath = child.Key;            // e.g., "B" or "B.C"
                var hostAlias = $"{alias}.{childAliasPath}"; // e.g., "Alias.B" or "Alias.B.C"
                var childModInfo = new ModuleInfo { Alias = hostAlias, SourcePath = child.Value.SourcePath };

                foreach (var kv in child.Value.Env)
                {
                    // Build fully qualified name: Alias::B[::C]::name
                    var composite = childAliasPath.Replace('.', ':'); // temp: "B.C" -> "B:C" to avoid double replace
                    composite = composite.Replace(':', ':'); // no-op; maintain structure
                    var nameInParent = string.Join("::", childAliasPath.Split('.')) + "::" + kv.Key; // "B::C::name"
                    var qualifiedName = Qualify(alias, nameInParent); // "Alias::B::C::name"

                    // Rewrite expressions to add parent alias in front of any module-local refs (including submodule refs)
                    var rewritten = RewriteQualifiedNames(kv.Value, alias, moduleNames);

                    childModInfo.Env[kv.Key] = rewritten;
                    _context[qualifiedName] = _evaluator.Evaluate(rewritten);
                    _contextUnevaluated[qualifiedName] = rewritten;
                    childModInfo.QualifiedKeys.Add(qualifiedName);
                    // Ensure parent unload also removes child-bound entries
                    mod.QualifiedKeys.Add(qualifiedName);
                }
                _modules[hostAlias] = childModInfo;
            }

            _modules[alias] = mod;
            var total = mod.Env.Count + _modules.Count(kv => kv.Key.StartsWith(alias + ".", StringComparison.Ordinal));
            return $"Module '{alias}' loaded with {mod.Env.Count} symbols and {total - mod.Env.Count} submodule(s) from '{resolvedPath}'";
        }
        finally
        {
            _moduleLoadStack.Remove(abs);
        }
    }

    private async Task<string> ModuleReloadAsync(string[] parts)
    {
        if (parts.Length < 2) return "Usage: :module reload Alias";
        var alias = parts[1];
        if (!_modules.TryGetValue(alias, out var mod)) return $"Module '{alias}' not found";
        // Unload then load (also unload any hierarchical children like Alias.*)
        UnloadAliasTree(alias);
        return await ModuleLoadAsync(new[] { "load", $"\"{mod.SourcePath}\"", "as", alias });
    }

    private string ModuleUnload(string[] parts)
    {
        if (parts.Length < 2) return "Usage: :module unload Alias";
        var alias = parts[1];
        if (!_modules.ContainsKey(alias)) return $"Module '{alias}' not found";
        UnloadAliasTree(alias);
        return $"Module '{alias}' unloaded";
    }

    private void UnloadAliasTree(string aliasRoot)
    {
        // Gather all aliases to remove: the root and any children with the dotted prefix
        var toRemove = _modules.Keys.Where(a => a.Equals(aliasRoot, StringComparison.Ordinal) || a.StartsWith(aliasRoot + ".", StringComparison.Ordinal)).ToList();
        // Collect all qualified keys referenced by these modules
        var allKeys = new HashSet<string>(StringComparer.Ordinal);
        foreach (var a in toRemove)
        {
            if (_modules.TryGetValue(a, out var m))
            {
                foreach (var k in m.QualifiedKeys) allKeys.Add(k);
            }
        }
        // Remove from contexts
        foreach (var k in allKeys)
        {
            _context.Remove(k);
            _contextUnevaluated.Remove(k);
        }
        // Remove modules
        foreach (var a in toRemove)
            _modules.Remove(a);
    }

    private string ModuleAlias(string[] parts)
    {
        // Rename alias reference in registry only (qualified bindings remain with old alias names)
        if (parts.Length < 4 || !string.Equals(parts[2], "as", StringComparison.OrdinalIgnoreCase))
            return "Usage: :module alias Old as New";
        var oldA = parts[1]; var newA = parts[3];
        if (!_modules.TryGetValue(oldA, out var mod)) return $"Module '{oldA}' not found";
        if (_modules.ContainsKey(newA)) return $"Module alias '{newA}' already exists";
        _modules.Remove(oldA);
        _modules[newA] = mod; // keep internal Alias unchanged; dictionary key is the source of truth
        return $"Module alias changed: {oldA} -> {newA} (note: existing qualified names not renamed)";
    }

    private string ModuleImport(string spec)
    {
        // Formats:
        //   Alias::{a,b,c}
        //   Alias::{a as x, b}
        spec = spec.Trim();
        int sep = spec.IndexOf("::", StringComparison.Ordinal);
        if (sep <= 0) return "Usage: :module import Alias::{a,b}";
    var alias = spec[..sep].Trim(); // supports dotted alias paths like A.B.C
    if (!_modules.TryGetValue(alias, out var mod)) return $"Module '{alias}' not found";
        var rest = spec[(sep + 2)..].Trim();
        if (!rest.StartsWith("{") || !rest.EndsWith("}")) return "Error: Expected {a,b,...}";
        var inner = rest[1..^1].Trim();
        if (string.IsNullOrWhiteSpace(inner)) return "Error: Empty import set";
        var items = inner.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        int imported = 0;
        foreach (var raw in items)
        {
            var item = raw.Trim();
            string fromName, toName;
            if (item.Contains(" as ", StringComparison.OrdinalIgnoreCase))
            {
                var parts2 = item.Split(" as ", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
                if (parts2.Length != 2) return $"Error: Bad import rename: {item}";
                fromName = parts2[0]; toName = parts2[1];
            }
            else { fromName = toName = item; }
            if (!mod.Env.TryGetValue(fromName, out var expr)) return $"Error: '{fromName}' not found in module {alias}";
            var qualified = _context.GetValueOrDefault(Qualify(alias, fromName));
            var value = qualified ?? _evaluator.Evaluate(expr);
            _context[toName] = value;
            _contextUnevaluated[toName] = expr;
            imported++;
        }
        return $"Imported {imported} from {alias}";
    }

    private async Task<string> ModuleWithAsync(string rest)
    {
        // :module with Alias => <expr>
        var tmp = rest.Trim();
        int arrow = tmp.IndexOf("=>", StringComparison.Ordinal);
        if (arrow <= 0) return "Usage: :module with Alias => <expr>";
        var alias = tmp[..arrow].Trim();
        var exprText = tmp[(arrow + 2)..].Trim();
        if (!_modules.TryGetValue(alias, out var mod)) return $"Module '{alias}' not found";
        // Temporarily import all symbols into scope, evaluate, then roll back
        var backup = new Dictionary<string, (Expr val, Expr uneval)>(_context.Count);
        foreach (var kv in mod.Env)
        {
            var qualified = _context.GetValueOrDefault(Qualify(alias, kv.Key));
            var value = qualified ?? _evaluator.Evaluate(kv.Value);
            if (_context.TryGetValue(kv.Key, out var prevVal) && _contextUnevaluated.TryGetValue(kv.Key, out var prevUneval))
                backup[kv.Key] = (prevVal, prevUneval);
            _context[kv.Key] = value;
            _contextUnevaluated[kv.Key] = kv.Value;
        }
        var (exp, str) = await ProcessInputAsync(exprText);
        // Restore
        foreach (var kv in mod.Env)
        {
            if (backup.TryGetValue(kv.Key, out var prev))
            { _context[kv.Key] = prev.val; _contextUnevaluated[kv.Key] = prev.uneval; }
            else
            { _context.Remove(kv.Key); _contextUnevaluated.Remove(kv.Key); }
        }
        return str;
    }

    private string ModuleClearImports()
    {
        // Remove any top-level names that were imported (heuristic: not containing '::')
        // Be conservative: keep native functions and qualified ones
        var toRemove = _contextUnevaluated.Keys.Where(k => !k.Contains("::")).ToList();
        int removed = 0;
        foreach (var k in toRemove)
        {
            if (_modules.Values.Any(m => m.QualifiedKeys.Contains(k))) continue; // avoid removing qualified module entries accidentally
            _context.Remove(k);
            _contextUnevaluated.Remove(k);
            removed++;
        }
        return $"Cleared {removed} imported names";
    }

    private static string Qualify(string alias, string name) => $"{alias}::{name}";

    // Walk expression tree and rewrite Var(name) that match module-local names to qualified alias::name
    private Expr RewriteQualifiedNames(Expr expr, string alias, HashSet<string> moduleNames)
    {
        Expr Walk(Expr e, HashSet<string> bound)
        {
            switch (e.Type)
            {
                case ExprType.Var:
                    if (e.VarName is null) return e;
                    // Qualify only module-defined free variables; leave external refs untouched
                    if (bound.Contains(e.VarName)) return e;
                    return moduleNames.Contains(e.VarName) ? Expr.Var(Qualify(alias, e.VarName)) : e;
                case ExprType.Abs:
                    var b = new HashSet<string>(bound);
                    if (e.AbsVarName != null) b.Add(e.AbsVarName);
                    return e.AbsBody is null ? e : Expr.Abs(e.AbsVarName!, Walk(e.AbsBody, b));
                case ExprType.App:
                    return Expr.App(Walk(e.AppLeft!, bound), Walk(e.AppRight!, bound));
                case ExprType.Thunk:
                    if (e.ThunkValue is null) return e;
                    var rewritten = Walk(e.ThunkValue.IsForced && e.ThunkValue.ForcedValue is not null ? e.ThunkValue.ForcedValue! : e.ThunkValue.Expression, bound);
                    return Expr.Thunk(rewritten, e.ThunkValue.Environment);
                case ExprType.YCombinator:
                    return e;
                default:
                    return e;
            }
        }
        return Walk(expr, new HashSet<string>());
    }

    private string HandleHistory(string arg)
    {
        int n = 20;
        if (!string.IsNullOrWhiteSpace(arg) && int.TryParse(arg, out var parsed) && parsed > 0)
            n = parsed;
        var span = _history.Count <= n ? _history : _history.TakeLast(n).ToList();
        if (span.Count == 0) return "# (history empty)";
        var width = (_history.Count).ToString().Length;
        var lines = span.Select((s, i) =>
        {
            int index = _history.Count - span.Count + i; // absolute index
            return $"  {index.ToString().PadLeft(width)}: {s}";
        });
        return string.Join('\n', lines);
    }

    private async Task<string> HandleRepeatAsync(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg)) return "Usage: :repeat <index | -k>";
        int idx;
        if (int.TryParse(arg, out var parsed))
        {
            if (parsed < 0)
            {
                idx = _history.Count + parsed; // -1 => last
            }
            else idx = parsed;
        }
        else return "Error: index must be integer (absolute or negative offset)";
        if (idx < 0 || idx >= _history.Count) return $"Error: history index out of range (0..{_history.Count - 1})";
        var cmd = _history[idx];
        var (expr, str) = await ProcessInputAsync(cmd);
        return $"# repeat[{idx}]: {cmd}\n{str}".TrimEnd();
    }

    private async Task<string> HandleReloadAsync()
    {
        if (string.IsNullOrEmpty(_lastLoadedFile)) return "No prior :load file to reload.";
        if (!File.Exists(_lastLoadedFile)) return $"Last file no longer exists: {_lastLoadedFile}";
        return await LoadFileAsync(_lastLoadedFile);
    }

    private string HandleLast()
        => _lastResultExpr is null ? "# (no last expression)" : FormatWithNumerals(_lastResultExpr);
    

    // Consolidated formatting method that uses the enhanced Expr.ToString()
    /// <summary>
    /// Formats an expression, optionally pretty-printing and displaying decoded Church numerals.
    /// </summary>
    public string FormatWithNumerals(Expr expr) => 
        expr.ToString(_prettyPrint, _prettyPrint ? new System.Func<Expr,int?>(ExtractChurchNumeralValue) : null);

    

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
    
    // Helper method to determine if an application is "simple" for categorization
    private bool IsSimpleApplication(Expr expr)
    {
        if (expr.Type != ExprType.App) return false;
        
        // Consider it simple if it's a direct application of known functions to constants/variables
        var depth = 0;
        var current = expr;
        
        while (current != null && current.Type == ExprType.App && depth < 3)
        {
            current = current.AppLeft;
            depth++;
        }
        
        // Simple if the final left-most element is a variable and depth is reasonable
        return current?.Type == ExprType.Var && depth <= 2;
    }

    private string ClearEnvironment()
    {
        _context.Clear();
        MemoClear(); // Reuse cache clearing logic
        _stats.Reset();
        return "Environment cleared.";
    }

    private string HandleClear(string arg)
    {
        var opt = arg.Trim().ToLowerInvariant();
        if (string.IsNullOrEmpty(opt) || opt == "all")
        {
            _parser._macros.Clear();
            _parser._infixOperators.Clear();
            // Re-add default infix operators after full clear
            _parser.DefineInfixOperator("|>", 1, "left");
            _parser.DefineInfixOperator(".", 9, "right");
            var envMsg = ClearEnvironment();
            return envMsg + " (macros & infix operators cleared)";
        }
        return opt switch
        {
            "macros" => ClearMacros(),
            "defs" => ClearDefinitions(),
            "ops" => ClearInfixOperators(),
            "cache" => MemoClear(),
            _ => $"Unknown clear target: {opt}. Use :clear [macros|defs|ops|cache|all]"
        };
    }

    private string ClearMacros()
    {
        int count = _parser._macros.Sum(kv => kv.Value.Count);
        _parser._macros.Clear();
        return $"Cleared {count} macro clause(s).";
    }

    private string ClearDefinitions()
    {
        int defCount = _context.Count + _contextUnevaluated.Count;
        _context.Clear();
        _contextUnevaluated.Clear();
        // Do not touch macros / ops / stats / caches unless requested
        return $"Cleared {defCount} definition(s).";
    }

    private string ClearInfixOperators()
    {
        int opCount = _parser._infixOperators.Count;
        _parser._infixOperators.Clear();
        // Restore defaults
        _parser.DefineInfixOperator("|>", 1, "left");
        _parser.DefineInfixOperator(".", 9, "right");
        return $"Cleared {opCount} infix operator(s); restored defaults (|>, .).";
    }

    private string TestClear()
    {
        _stats.StructEqCalls = 0;
        _stats.StructEqSuccesses = 0;
        _testRecords.Clear();
        return "Test counters cleared (structural equality).";
    }

    private string TestResult()
    {
        if (_testOutputMode == TestOutputMode.Json)
        {
            // Derive pass/fail counts. _stats.StructEqSuccesses doesn't accurately track passes for non-alpha kinds (by design),
            // so prefer counting recorded test objects when available.
            int passCount = _testRecords.Count > 0 ? _testRecords.Count(r => r.Passed) : _stats.StructEqSuccesses;
            int failCount = _stats.StructEqCalls - passCount;
            var obj = new
            {
                mode = "json",
                totalCalls = _stats.StructEqCalls,
                successes = _stats.StructEqSuccesses,
                passCount,
                failCount,
                successRate = _stats.StructEqCalls == 0 ? 0 : (100.0 * _stats.StructEqSuccesses / _stats.StructEqCalls),
                tests = _testRecords.Select(r => new
                {
                    kind = r.Kind,
                    passed = r.Passed,
                    left = r.Left,
                    right = r.Right,
                    callIndex = r.CallIndex,
                    successRate = r.SuccessRate,
                    line = r.Line,
                    file = r.File,
                    fileLine = r.FileLine,
                    // macroExpandedLine removed
                }).ToArray()
            };
            return System.Text.Json.JsonSerializer.Serialize(obj, new System.Text.Json.JsonSerializerOptions { WriteIndented = true });
        }
        return $"Test results: structural equality calls={_stats.StructEqCalls}, successes={_stats.StructEqSuccesses}, success rate={( _stats.StructEqCalls==0 ? 0 : (100.0*_stats.StructEqSuccesses/_stats.StructEqCalls)):F1}%";
    }

    private string SetTestOutputMode(TestOutputMode mode)
    {
        _testOutputMode = mode;
        return $"Test output mode set to {mode.ToString().ToLowerInvariant()}";
    }

    internal void RecordTestResult(string kind, bool passed, Expr leftNorm, Expr rightNorm)
    {
        if (_testOutputMode != TestOutputMode.Json) return; // Only store details when JSON mode
        var callIndex = _stats.StructEqCalls; // already incremented before logging summary
        var successRate = _stats.StructEqCalls == 0 ? 0 : (100.0 * _stats.StructEqSuccesses / _stats.StructEqCalls);
    _testRecords.Add(new TestRecord(kind, passed, FormatWithNumerals(leftNorm), FormatWithNumerals(rightNorm), callIndex, successRate, _currentInputLine, _currentSourceFile, _currentSourceLine));
    }

    internal Expr NormalizeExpression(Expr expr)
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
                ExprType.Var =>
                    // Attempt to inline top-level combinator / function definitions so that
                    // normalization can perform beta-reduction across variable references.
                    // This lets (K 42 99) normalize to 42 instead of remaining as K 42 99
                    // (previously a Var prevented seeing the underlying λ abstraction).
                    // Only inline simple Abs bodies to avoid expanding large data accidentally.
                    (expr.VarName is not null
                        && _context.TryGetValue(expr.VarName, out var bound)
                        && bound.Type == ExprType.Abs)
                        ? bound
                        : expr,
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

    
}
