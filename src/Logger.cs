using System.ComponentModel.Design;
using System.Drawing;
using System.Text.RegularExpressions;
using System.Linq;
using System.Runtime.InteropServices;

namespace LambdaCalculus;

public class Logger
{
    private string _logFile = "";
    private StreamWriter? _logWriter;
    private readonly SemaphoreSlim _logFileLock = new(1);
    private readonly object _bufferLock = new();
    private readonly List<string> _buffer = new();

    /// <summary>
    /// When true, all log messages (plain text, without ANSI colors) are also captured in memory.
    /// </summary>
    public bool EnableBuffering { get; set; } = false;

    /// <summary>
    /// If false, console output is suppressed (still written to file / buffer if enabled).
    /// </summary>
    public bool ConsoleOutputEnabled { get; set; } = true;

    private const string RED = "\u001b[31m";
    private const string GREEN = "\u001b[32m";
    private const string YELLOW = "\u001b[33m";
    private const string BLUE = "\u001b[34m";
    private const string MAGENTA = "\u001b[35m";
    private const string BLACK = "\u001b[30m";
    private const string CYAN = "\u001b[36m";
    private const string WHITE = "\u001b[37m";
    private const string GRAY = "\u001b[90m";
    private const string ORANGE = "\u001b[38;5;214m";
    private const string PINK = "\u001b[38;5;205m";

    private const string RESET = "\u001b[0m";

    public static string Prompt(string txt) => $"{CYAN}{txt}{RESET} ";
    public string LogStatus => _logFile == "" ? "DISABLED" : _logFile;

    public async Task<string> HandleLogCommandAsync(string arg) => arg switch
    {
        "off" or "" => (_logFile = "", "Logging is disabled.").Item2,
        "clear" => await ClearLogFileAsync(),
        _ => (_logFile = arg, $"Logging is enabled to '{arg}'").Item2
    };

    public async Task<string> ClearLogFileAsync()
    {
        try
        {
            await CloseLogFileAsync();
            await File.WriteAllTextAsync(_logFile, "");
            return $"Log file '{_logFile}' cleared.";
        }
        catch (Exception ex)
        {
            return $"Error: clearing log file: {ex.Message}";
        }
    }

    public async Task CloseLogFileAsync()
    {
        if (_logWriter is null) return;
        await _logWriter.DisposeAsync();
        _logWriter = null;
    }

    private static string GetColor(string message) => message switch
    {
        string s when s.StartsWith("Error:") => RED,
        string s when s.StartsWith("#") => YELLOW,            // Comments
        string s when s.StartsWith("->") => GREEN,            // Results/Assignments
        string s when s.StartsWith("Step") => YELLOW,         // Evaluation steps
        string s when s.StartsWith("Time:") => BLUE,          // Timing info
        string s when s.StartsWith("Name:") => BLUE,          // Final result details
        string s when s.StartsWith("Eval:") => MAGENTA,       // Evaluation expression
        string s when s.StartsWith("Macro") => BLUE,          // Macro expansion
        string s when s.StartsWith("Test: left") => MAGENTA,  // Test expression
        string s when s.StartsWith("Test: right") => MAGENTA, // Test expression
        string s when s.StartsWith("Test: passed") => GREEN,  // expected test result
        string s when s.StartsWith("Test: failed") => RED,    // expected test result
        string s when s.StartsWith(":") => PINK,              // General command
        string s when s.Contains("Loading") => CYAN,          // Loading files
        string s when s.Contains("<<") => GRAY,               // Reading file lines
        string s when s.Contains(">>") => GREEN,              // Result of reading file lines
        _ => RESET                                            // Default
    };

    static readonly List<string> commands =
    [
        ":clear ", ":infix ", ":lazy ", ":load ", ":log ",
        ":macro ", ":memo ", ":native ", "pretty ", ":stats ", ":test "
    ];

    public static void LogToConsole(string msg)
    {
        var color = GetColor(msg);

        var sectionTexts = msg
            .Split(';', StringSplitOptions.RemoveEmptyEntries)
            .Select(section =>
            {
                var cInd = section.IndexOf('#');
                var match = commands
                    .Select(cmd => (cmd, idx: section.IndexOf(cmd)))
                    .FirstOrDefault(t => t.idx >= 0 && (cInd < 0 || t.idx < cInd));
                var txt = match.cmd != null
                    ? section[..match.idx] + GetColor(match.cmd) + section[match.idx..] + RESET
                    : section;
                if (cInd >= 0)
                    txt = txt[..cInd] + YELLOW + txt[cInd..] + RESET;
                return $"{color}{txt}{RESET}";
            });

        var text = string.Join($"{GREEN};{RESET}", sectionTexts);
        Console.WriteLine($"{text}");
    }

    public void ClearBuffer()
    {
        lock (_bufferLock) _buffer.Clear();
    }

    public IReadOnlyList<string> GetBufferSnapshot()
    {
        lock (_bufferLock) return _buffer.ToArray();
    }

    public async Task LogAsync(string message, bool toConsole = true)
    {
        if (EnableBuffering)
        {
            lock (_bufferLock) _buffer.Add(message);
        }
        if (toConsole && ConsoleOutputEnabled) LogToConsole(message);
        if (string.IsNullOrWhiteSpace(_logFile)) return;

        await _logFileLock.WaitAsync();
        try
        {
            _logWriter ??= new StreamWriter(_logFile, append: true, encoding: System.Text.Encoding.UTF8);
            await _logWriter.WriteLineAsync(message);
        }
        catch (Exception ex)
        {
            LogToConsole($"Error: writing to log file: {ex.Message}");
        }
        _logFileLock.Release();
    }

    public void Log(string message, bool toConsole = true)
    {
        LogAsync(message, toConsole)
            .GetAwaiter()
            .GetResult();
    }
}
