using System.ComponentModel.Design;
using System.Drawing;
using System.Text.RegularExpressions;
using System.Linq;
using System.Runtime.InteropServices;

namespace LambdaCalculus;

/// <summary>
/// Central logging utility supporting colored console output, optional file logging,
/// in-memory buffering, and reactive subscriptions for streaming to external clients.
/// </summary>
public class Logger
{
    private string _logFile = "";
    private StreamWriter? _logWriter;
    private readonly SemaphoreSlim _logFileLock = new(1);
    private readonly object _bufferLock = new();
    private readonly List<string> _buffer = new();
    private readonly List<Action<string>> _subscribers = new();
    private readonly object _subLock = new();

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

    /// <summary>Formats a prompt string with ANSI color (if console supports it).</summary>
    public static string Prompt(string txt) => $"{CYAN}{txt}{RESET} ";
    /// <summary>Returns current log file path or DISABLED.</summary>
    public string LogStatus => _logFile == "" ? "DISABLED" : _logFile;

    /// <summary>Handles :log command arguments (off|clear|filename).</summary>
    public async Task<string> HandleLogCommandAsync(string arg) => arg switch
    {
        "off" or "" => (_logFile = "", "Logging is disabled.").Item2,
        "clear" => await ClearLogFileAsync(),
        _ => (_logFile = arg, $"Logging is enabled to '{arg}'").Item2
    };

    /// <summary>Clears current log file contents.</summary>
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

    /// <summary>Closes the open log file writer (if any).</summary>
    public async Task CloseLogFileAsync()
    {
        if (_logWriter is null) return;
        await _logWriter.DisposeAsync();
        _logWriter = null;
    }

    private static string GetColor(string message) => message switch
    {
        string s when s.StartsWith("Error:") => RED,
        string s when s.StartsWith("#") => YELLOW,       // Comments
        string s when s.StartsWith("->") => GREEN,       // Results/Assignments
        string s when s.StartsWith("Step") => YELLOW,    // Evaluation steps
        string s when s.StartsWith("Time:") => BLUE,     // Timing info
        string s when s.StartsWith("Name:") => BLUE,     // Final result details
        string s when s.StartsWith("Eval:") => MAGENTA,  // Evaluation expression
        string s when s.StartsWith("Print ") => ORANGE,  // Print helper output
        string s when s.StartsWith("Macro") => BLUE,     // Macro expansion
        string s when s.StartsWith("Test:") => s.Contains("passed") ? GREEN : s.Contains("failed") ? RED : MAGENTA,  // Alpha test result
        string s when s.StartsWith(":") => PINK,         // General command
        string s when s.Contains("Loading") => CYAN,     // Loading files
        string s when s.Contains("<<") => GRAY,          // Reading file lines
        string s when s.Contains(">>") => GREEN,         // Result of reading file lines
        _ => RESET                                       // Default
    };

    // Matches literal sequences like \u03BB
    private static readonly Regex UnicodeEscapeRegex = new("\\\\u([0-9a-fA-F]{4})", RegexOptions.Compiled);
    private static string DecodeUnicodeEscapes(string s)
    {
        if (string.IsNullOrEmpty(s) || !s.Contains("\\u")) return s;
        return UnicodeEscapeRegex.Replace(s, m =>
        {
            try
            {
                var code = Convert.ToInt32(m.Groups[1].Value, 16);
                return char.ConvertFromUtf32(code);
            }
            catch { return m.Value; }
        });
    }

    static readonly List<string> commands =
    [
        ":clear ", ":infix ", ":lazy ", ":load ", ":log ",
        ":macro ", ":memo ", ":native ", "pretty ", ":stats ", ":test "
    ];

    /// <summary>Writes a single colored line to the console.</summary>
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

    /// <summary>Clears the in-memory buffer of captured log lines.</summary>
    public void ClearBuffer()
    {
        lock (_bufferLock) _buffer.Clear();
    }

    /// <summary>Returns a snapshot copy of buffered log lines.</summary>
    public IReadOnlyList<string> GetBufferSnapshot()
    {
        lock (_bufferLock) return _buffer.ToArray();
    }

    /// <summary>Logs a message asynchronously (buffer, console, file as configured).</summary>
    public async Task LogAsync(string message, bool toConsole = true)
    {
        if (message is null) return;
        var lines = message.Replace("\r\n", "\n").Replace('\r', '\n').Split('\n');
        foreach (var rawLine in lines)
        {
            var line = rawLine; // preserve empty string lines
            if (EnableBuffering)
            {
                lock (_bufferLock) _buffer.Add(line);
            }
            // Notify subscribers per line (web UI expects one logical line at a time)
            Action<string>[] subs;
            lock (_subLock) subs = _subscribers.ToArray();
            foreach (var s in subs)
            {
                try { s(line); } catch { /* ignore subscriber errors */ }
            }
        if (toConsole && ConsoleOutputEnabled) LogToConsole(DecodeUnicodeEscapes(line));
            if (!string.IsNullOrWhiteSpace(_logFile))
            {
                await _logFileLock.WaitAsync();
                try
                {
                    _logWriter ??= new StreamWriter(_logFile, append: true, encoding: System.Text.Encoding.UTF8);
            await _logWriter.WriteLineAsync(DecodeUnicodeEscapes(line));
                }
                catch (Exception ex)
                {
                    LogToConsole($"Error: writing to log file: {ex.Message}");
                }
                _logFileLock.Release();
            }
        }
    }

    /// <summary>Synchronous wrapper for <see cref="LogAsync"/>.</summary>
    public void Log(string message, bool toConsole = true)
    {
        LogAsync(message, toConsole)
            .GetAwaiter()
            .GetResult();
    }

    /// <summary>
    /// Subscribes to log messages. Returns an IDisposable that, when disposed, unsubscribes.
    /// </summary>
    public IDisposable Subscribe(Action<string> handler)
    {
        lock (_subLock) _subscribers.Add(handler);
        return new Unsubscriber(_subscribers, handler, _subLock);
    }

    private sealed class Unsubscriber : IDisposable
    {
        private readonly List<Action<string>> _list;
        private readonly Action<string> _handler;
        private readonly object _lock;
        private bool _disposed;
        public Unsubscriber(List<Action<string>> list, Action<string> handler, object l)
        { _list = list; _handler = handler; _lock = l; }
        public void Dispose()
        {
            if (_disposed) return;
            lock (_lock) _list.Remove(_handler);
            _disposed = true;
        }
    }
}
