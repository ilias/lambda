namespace LambdaCalculus;

public partial class Interpreter
{
    // --- Documentation & discovery --------------------------------------------------------
    private readonly Dictionary<string, string> _docs = new(StringComparer.Ordinal);

    private string HandleDoc(string arg)
    {
        // :doc <name>
        // :doc <name> = "text"
        // :doc export <file>
        if (string.IsNullOrWhiteSpace(arg))
            return "Usage: :doc <name> | :doc <name> = \"text\" | :doc export <file>";

        var parts = arg.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length >= 1 && parts[0].Equals("export", StringComparison.OrdinalIgnoreCase))
        {
            var file = parts.Length > 1 ? parts[1].Trim() : "docs.md";
            try
            {
                var lines = new List<string> { "# Documentation", "" };
                foreach (var kv in _docs.OrderBy(k => k.Key))
                {
                    lines.Add($"## {kv.Key}");
                    lines.Add("");
                    lines.Add(kv.Value);
                    lines.Add("");
                }
                File.WriteAllLines(file, lines);
                return $"Exported {_docs.Count} doc entries to {file}";
            }
            catch (Exception ex) { return $"Error exporting docs: {ex.Message}"; }
        }

        var eq = arg.IndexOf('=');
        if (eq > 0)
        {
            var name = arg[..eq].Trim();
            var value = arg[(eq + 1)..].Trim();
            value = value.Trim();
            if (value.StartsWith('"') && value.EndsWith('"') && value.Length >= 2)
                value = value[1..^1];
            _docs[name] = value;
            return $"Doc updated: {name}";
        }
        else
        {
            var name = arg.Trim();
            if (_docs.TryGetValue(name, out var doc))
                return doc;
            // Fallback: show a short descriptor if exists
            var sb = new System.Text.StringBuilder();
            if (_contextUnevaluated.TryGetValue(name, out var expr))
                sb.AppendLine($"def {name} = {FormatWithNumerals(expr)}");
            else if (_contextUnevaluated.TryGetValue(name.Replace("::", ":"), out var expr2))
                sb.AppendLine($"def {name} = {FormatWithNumerals(expr2)}");
            if (_parser._macros.ContainsKey(name)) sb.AppendLine("macro: defined");
            if (_nativeFunctions.ContainsKey(name)) sb.AppendLine("native: defined");
            if (_parser._infixOperators.ContainsKey(name))
            {
                var op = _parser._infixOperators[name];
                sb.AppendLine($"infix: prec {op.Precedence} {op.Associativity.ToString().ToLower()}");
            }
            var m = _modules.FirstOrDefault(kv => kv.Value.Env.ContainsKey(name));
            if (!string.IsNullOrEmpty(m.Key)) sb.AppendLine($"module member: {m.Key}::{name}");
            return sb.Length == 0 ? $"No doc for {name}" : sb.ToString().TrimEnd();
        }
    }

    private string HandleFind(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg)) return "Usage: :find <name>";
        var name = arg.Trim();
        var lines = new List<string>();
        if (_contextUnevaluated.ContainsKey(name)) lines.Add($"top-level: {name}");
        foreach (var (alias, mod) in _modules)
            if (mod.Env.ContainsKey(name)) lines.Add($"module: {alias}::{name}");
        if (_parser._macros.ContainsKey(name)) lines.Add("macro: defined");
        if (_nativeFunctions.ContainsKey(name)) lines.Add("native: defined");
        if (_parser._infixOperators.ContainsKey(name)) lines.Add("infix: defined");
        return lines.Count == 0 ? $"Not found: {name}" : string.Join('\n', lines);
    }

    private string HandleGrep(string arg)
    {
        if (string.IsNullOrWhiteSpace(arg)) return "Usage: :grep <pattern>";
        var pat = arg.Trim();
        var comp = StringComparison.OrdinalIgnoreCase;
        var lines = new List<string>();
        lines.AddRange(_contextUnevaluated.Keys.Where(k => k.Contains(pat, comp)).OrderBy(s => s).Select(s => $"def: {s}"));
        foreach (var (alias, mod) in _modules)
        {
            lines.AddRange(mod.Env.Keys.Where(k => k.Contains(pat, comp)).OrderBy(s => s).Select(s => $"mod {alias}::{s}"));
        }
        lines.AddRange(_parser._macros.Keys.Where(k => k.Contains(pat, comp)).OrderBy(s => s).Select(s => $"macro: {s}"));
        lines.AddRange(_nativeFunctions.Keys.Where(k => k.Contains(pat, comp)).OrderBy(s => s).Select(s => $"native: {s}"));
        lines.AddRange(_parser._infixOperators.Keys.Where(k => k.Contains(pat, comp)).OrderBy(s => s).Select(s => $"infix: {s}"));
        return lines.Count == 0 ? "No matches" : string.Join('\n', lines);
    }
    // Command handling helpers extracted from Interpreter
    private string HandleNativeArithmetic(string arg)
    {
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

    private string HandleStrategy(string arg)
    {
        // :strategy cbv | need | lazy | default
        var choice = (arg ?? "").Trim().ToLowerInvariant();
        switch (choice)
        {
            case "cbv": // call-by-value
            case "call-by-value":
                _evaluator = new CEKEvaluator(this);
                _lazyEvaluation = false;
                return "Strategy: CEK (call-by-value)";
            case "need": // call-by-need (lazy + memoization via thunks)
            case "call-by-need":
            case "lazy":
                _evaluator = new CEKEvaluator(this);
                _lazyEvaluation = true;
                return "Strategy: CEK (call-by-need)";
            case "default":
            case "cek":
                _evaluator = new CEKEvaluator(this);
                return $"Strategy: CEK ({(_lazyEvaluation ? "call-by-need" : "call-by-value")})";
            default:
                return "Usage: :strategy <cbv|need|lazy|cek|default>";
        }
    }

    private string HandleSteps()
    {
        return $"Steps: {_stats.Iterations:#,##0}";
    }

    private string HandleTime(string arg)
    {
        _showTime = arg != "off";
        return $"Time display {(_showTime ? "enabled" : "disabled")}";
    }

    private string HandleBinder(string arg)
    {
        // :binder debruijn on|off
        var parts = (arg ?? "").Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        if (parts.Length == 0)
            return $"Binder mode: {(_useDeBruijnBinder ? "debruijn" : "named")}";
        if (!string.Equals(parts[0], "debruijn", StringComparison.OrdinalIgnoreCase))
            return "Usage: :binder debruijn on|off";
        if (parts.Length == 1)
            return $"Binder mode: {(_useDeBruijnBinder ? "debruijn" : "named")}";
        var on = string.Equals(parts[1], "on", StringComparison.OrdinalIgnoreCase);
        _useDeBruijnBinder = on;
        return $"Binder: {(_useDeBruijnBinder ? "debruijn" : "named")}";
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
        if (!string.IsNullOrWhiteSpace(arg))
        {
            var hash = arg.IndexOf('#');
            if (hash >= 0) arg = arg[..hash].TrimEnd();
        }
        if (string.IsNullOrWhiteSpace(arg)) return ShowInfixOperators();
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
        try { return _parser.ParseAndDefineMacro(arg); }
        catch (Exception ex) { return $"Error defining macro: {ex.Message}"; }
    }
}
