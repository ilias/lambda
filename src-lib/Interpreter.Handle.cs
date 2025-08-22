namespace LambdaCalculus;

public partial class Interpreter
{
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
