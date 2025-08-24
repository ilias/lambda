namespace LambdaCalculus;

public partial class Interpreter
{
    private sealed record CommandInfo(string Key, string Syntax, string Description);

    // Single source of truth for command metadata
    private static readonly List<CommandInfo> _commandMetadata = new()
    {
        new(":clear", ":clear", "Clear environment, statistics and caches"),
        new(":depth", ":depth [n]", "Show or set maximum recursion depth (range 10-10000)"),
        new(":env", ":env", "Display current top-level definitions (environment)"),
        new(":exit", ":exit | :quit", "Exit the interpreter"),
        new(":help", ":help", "Show help summary"),
        new(":infix", ":infix [op prec assoc]", "Define or list infix operators (assoc = left|right)"),
        new(":lazy", ":lazy on|off", "Toggle lazy (on) vs eager (off) evaluation"),
        new(":load", ":load <file>", "Load a .lambda file (may contain defs, macros, infix)"),
        new(":log", ":log <file|off|clear>", "Log output to file, disable or clear current file"),
        new(":macro", ":macro (<pattern>) => <body>", "Define a macro clause (supports guards & rest)"),
        new(":macros", ":macros", "List all macro clauses"),
        new(":memo", ":memo", "Clear all memoization caches"),
        new(":multiline", ":multiline", "Show multi-line input help"),
        new(":native", ":native on|off|show", "Toggle native arithmetic or list native primitives"),
        new(":pretty", ":pretty on|off | :pp on|off", "Toggle pretty printing (numerals, lists, booleans)"),
        new(":save", ":save <file>", "Persist current environment to a file"),
        new(":stats", ":stats", "Show performance statistics & cache metrics"),
        new(":step", ":step on|off", "Toggle step-by-step CEK trace output"),
        new(":test", ":test clear | :test result", "Reset or display structural equality test counters"),
        new(":commands", ":commands", "Output all commands as a markdown table (for README sync)")
    };

    private static string BuildCommandsMarkdown()
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine("| Command | Syntax | Description |");
        sb.AppendLine("|---------|--------|-------------|");
        foreach (var group in _commandMetadata.GroupBy(c => c.Key).OrderBy(c => c.Key))
        {
            foreach (var c in group)
            {
                sb.AppendLine($"| {c.Key} | `{c.Syntax}` | {c.Description} |");
            }
        }
        return sb.ToString();
    }

    private static string BuildCommandsPlain()
    {
        var maxSyntax = _commandMetadata.Max(c => c.Syntax.Length);
        var lines = _commandMetadata
            .Select(c => $"  {c.Syntax.PadRight(maxSyntax)}  {c.Description}")
            .ToArray();
        return string.Join("\n", lines);
    }
}
