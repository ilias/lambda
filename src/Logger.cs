namespace LambdaCalculus;

public class Logger
{
    private string _logFile = "";
    private StreamWriter? _logWriter;
    private readonly SemaphoreSlim _logFileLock = new(1);

    private const string RED = "\u001b[31m";
    private const string GREEN = "\u001b[32m";
    private const string YELLOW = "\u001b[33m";
    private const string BLUE = "\u001b[34m";
    private const string MAGENTA = "\u001b[35m";
    private const string CYAN = "\u001b[36m";
    private const string WHITE = "\u001b[37m";
    private const string GRAY = "\u001b[90m";

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

    private static string GetMessageColor(string message) => message switch
    {
        string s when s.StartsWith("Error:") => RED,
        string s when s.StartsWith("#") => YELLOW,      // Comments
        string s when s.StartsWith("->") => GREEN,      // Results/Assignments
        string s when s.StartsWith("Step") => YELLOW,   // Evaluation steps
        string s when s.StartsWith("Time:") => BLUE,    // Timing info
        string s when s.StartsWith("Name:") => BLUE,    // Final result details
        string s when s.StartsWith("Eval:") => MAGENTA, // Evaluation expression
        string s when s.StartsWith("Test: good:") => GREEN,     // Test expression
        string s when s.StartsWith("Test: expected:") => RED,     // Test expression
        string s when s.StartsWith("Test: result:") => RED,     // expected test result
        string s when s.Contains("Loading") => CYAN,    // Loading files
        string s when s.Contains("<<") => GRAY,         // Reading file lines
        string s when s.Contains(">>") => GREEN,        // Result of reading file lines
        _ => RESET                                      // Default
    };

    public static void LogToConsole(string message) =>
        Console.WriteLine($"{GetMessageColor(message)}{message.Replace("\t", RESET)}{RESET}");

    public async Task LogAsync(string message, bool toConsole = true)
    {
        if (toConsole) LogToConsole(message);
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

    public void Log(string message, bool toConsole = true) =>
        LogAsync(message, toConsole)
            .GetAwaiter()
            .GetResult(); // Synchronous version for compatibility with existing code
}
