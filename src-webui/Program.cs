using LambdaCalculus;
using System.IO;

var builder = WebApplication.CreateBuilder(args);
var services = builder.Services;

services.AddSingleton<Interpreter>(_ =>
{
    var logger = new Logger { EnableBuffering = true, ConsoleOutputEnabled = false };
    var interp = new Interpreter(logger);
    // Attempt to preload stdlib searching common relative locations
    var candidates = new[]
    {
        "stdlib.lambda",                    // working directory
        Path.Combine("..", "stdlib.lambda"), // parent (repo root when running from src-webui)
        Path.Combine(AppContext.BaseDirectory, "stdlib.lambda") // output directory
    };
    foreach (var c in candidates)
    {
        if (File.Exists(c))
        {
            var msg = interp.LoadFileIfExistsAsync(c).GetAwaiter().GetResult();
            Console.WriteLine($"[startup] {msg}");
            break;
        }
    }
    interp.EnsureRangeBuiltinsAsync().GetAwaiter().GetResult();
    return interp;
});

var app = builder.Build();
app.UseDefaultFiles();
app.UseStaticFiles();

app.MapGet("/api/eval", async (string expr, Interpreter interp) =>
{
    if (interp is null) return Results.Problem("Interpreter not available");
    // Clear previous buffer using public accessor
    interp.Logger.ClearBuffer();
    var sw = System.Diagnostics.Stopwatch.StartNew();
    var result = await interp.ProcessInputAsync(expr);
    sw.Stop();
    // Reproduce CLI style output lines (Name/Time/iterations + output) into logger
    await interp.DisplayOutput(result, sw.Elapsed);
    var logs = interp.Logger.GetBufferSnapshot();
    return Results.Ok(new { input = expr, output = result.str, normalized = result.exp?.ToString(), logs });
});

app.MapPost("/api/load", async (LoadFileRequest req, Interpreter interp) =>
{
    interp.Logger.ClearBuffer();
    var path = req.Path;
    if (!File.Exists(path) && Path.GetFileName(path).Equals("stdlib.lambda", StringComparison.OrdinalIgnoreCase))
    {
        var altCandidates = new[]
        {
            Path.Combine("..", "stdlib.lambda"),
            Path.Combine(AppContext.BaseDirectory, "stdlib.lambda")
        };
        foreach (var c in altCandidates)
        {
            if (File.Exists(c)) { path = c; break; }
        }
    }
    var result = await interp.LoadFileIfExistsAsync(path);
    var logs = interp.Logger.GetBufferSnapshot();
    return Results.Ok(new { message = result, logs });
});

// Health endpoint (prefixed for API namespace consistency)
app.MapGet("/api/health", () => Results.Ok(new { status = "ok" }));

app.Run();

/// <summary>
/// Request payload for /api/load specifying a file system path to a .lambda source file.
/// </summary>
public record LoadFileRequest(string Path);

// Expose Program for WebApplicationFactory in integration tests
public partial class Program { }
