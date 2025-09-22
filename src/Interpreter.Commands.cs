namespace LambdaCalculus;

public partial class Interpreter
{
    private sealed record CommandInfo(string Key, string Syntax, string Description);

    // Single source of truth for command metadata
    private static readonly List<CommandInfo> _commandMetadata = new()
    {
        new(":clear", ":clear [macros|defs|ops|cache|all]", "Clear state (default=all): macros, defs, ops (infix), cache (memoization), or all (env+macros+ops+stats+caches)"),
        new(":depth", ":depth [n]", "Show or set maximum recursion depth (range 10-10000)"),
        new(":env", ":env [defs|modules|macros|infix|native|all]", "Display environment subsets; default all"),
        new(":exit", ":exit | :quit", "Exit the interpreter"),
        new(":help", ":help", "Show help summary"),
        new(":infix", ":infix [op prec assoc]", "Define or list infix operators (assoc = left|right)"),
        new(":lazy", ":lazy on|off", "Toggle lazy (on) vs eager (off) evaluation"),
        new(":load", ":load <file>", "Load a .lambda file (may contain defs, macros, infix)"),
        new(":log", ":log <file|off|clear>", "Log output to file, disable or clear current file"),
        new(":macro", ":macro (<pattern>) => <body>", "Define a macro clause (supports guards & rest)"),
        new(":native", ":native on|off|show", "Toggle native arithmetic or list native primitives"),
        new(":pretty", ":pretty on|off | :pp on|off", "Toggle pretty printing (numerals, lists, booleans)"),
        new(":save", ":save <file>", "Persist current environment to a file"),
        new(":stats", ":stats", "Show performance statistics & cache metrics"),
        new(":step", ":step on|off", "Toggle step-by-step CEK trace output"),
        new(":test", ":test clear | :test result | :test json | :test text", "Reset counters, show results, or change result output mode (text/json)"),
        new(":module", ":module <load|reload|unload|list|import|alias|with|clear-imports> ...", "Module system: load/list/reload/unload/import/alias/with/clear-imports"),
    new(":doc", ":doc <name> | :doc <name> = \"text\" | :doc export <file>", "Show/set/export documentation for symbols (supports qualified names)"),
    new(":find", ":find <name>", "Find where a symbol is defined (top-level, module, macro, native, infix)"),
    new(":grep", ":grep <pattern>", "Search names across defs, modules, macros, natives, infix (case-insensitive substring)"),
        // QoL additions
        new(":hist", ":hist [n]", "Show last n (default 20) entered top-level inputs"),
        new(":repeat", ":repeat <index| -k>", "Re-run history entry by index (as shown in :hist) or negative offset (-1 = last)"),
        new(":reload", ":reload", "Reload the most recently :load'ed file"),
        new(":last", ":last", "Show the last evaluated expression's pretty-printed form again"),
    };

    // Command listing helpers removed (commands now documented directly in :help).
}
