using LambdaCalculus;
using System.IO;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System.Collections.Concurrent;
using System.Threading.Channels;

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
    return interp;
});

var app = builder.Build();
// --- Security Headers (CSP) --------------------------------------------------
// We remove all inline <script> / <style> blocks in index.html so we can enforce
// a reasonably strict Content-Security-Policy without using 'unsafe-inline'.
// If you later introduce inline styles/scripts, adjust CSP accordingly.
app.Use(async (ctx, next) =>
{
    const string csp = "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; font-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'; form-action 'self'";
    if (!ctx.Response.Headers.ContainsKey("Content-Security-Policy"))
    {
        ctx.Response.Headers["Content-Security-Policy"] = csp;
    }
    await next();
});

app.UseDefaultFiles();
app.UseStaticFiles();
app.UseWebSockets();

// SSE endpoint
app.MapGet("/api/stream", async (HttpContext ctx, Interpreter interp) =>
{
    ctx.Response.Headers.CacheControl = "no-cache";
    ctx.Response.Headers.Connection = "keep-alive";
    ctx.Response.Headers["X-Accel-Buffering"] = "no"; // for nginx proxies (no buffering)
    ctx.Response.ContentType = "text/event-stream";
    var response = ctx.Response;
    var logger = interp.Logger;
    var cancelled = ctx.RequestAborted;
    await response.Body.FlushAsync();
    var queue = new ConcurrentQueue<string>();
    using var sub = logger.Subscribe(line => queue.Enqueue(line));
    var pingBytes = System.Text.Encoding.UTF8.GetBytes("event: ping\ndata: .\n\n");
    while (!cancelled.IsCancellationRequested)
    {
        // Drain queue
        while (queue.TryDequeue(out var line))
        {
            try
            {
                var data = $"data: {line.Replace("\r", " ").Replace("\n", " ")}\n\n";
                var bytes = System.Text.Encoding.UTF8.GetBytes(data);
                await response.Body.WriteAsync(bytes, 0, bytes.Length, cancelled);
            }
            catch { cancelled = new System.Threading.CancellationToken(true); break; }
        }
        try
        {
            await response.Body.WriteAsync(pingBytes, 0, pingBytes.Length, cancelled);
            await response.Body.FlushAsync(cancelled);
        }
        catch { break; }
        await Task.Delay(1000, cancelled).ContinueWith(_ => { });
    }
});

// WebSocket endpoint for log streaming
app.Map("/ws", async ctx =>
{
    if (!ctx.WebSockets.IsWebSocketRequest)
    {
        ctx.Response.StatusCode = 400;
        await ctx.Response.WriteAsync("WebSocket connection expected");
        return;
    }
    using var socket = await ctx.WebSockets.AcceptWebSocketAsync();
    var logger = ctx.RequestServices.GetRequiredService<Interpreter>().Logger;
    var queue = Channel.CreateUnbounded<string>(new UnboundedChannelOptions { SingleReader = true, SingleWriter = false });
    using var sub = logger.Subscribe(line =>
    {
        // Try enqueue; ignore if channel closed
        queue.Writer.TryWrite(line);
    });
    var cancel = ctx.RequestAborted;
    // Consumer loop
    try
    {
        while (!cancel.IsCancellationRequested && socket.State == System.Net.WebSockets.WebSocketState.Open)
        {
            while (await queue.Reader.WaitToReadAsync(cancel))
            {
                while (queue.Reader.TryRead(out var line))
                {
                    var msg = System.Text.Encoding.UTF8.GetBytes(line);
                    await socket.SendAsync(msg, System.Net.WebSockets.WebSocketMessageType.Text, true, cancel);
                }
                break; // yield to check socket state / cancellation
            }
            await Task.Delay(100, cancel).ContinueWith(_ => { });
        }
    }
    catch { /* ignore connection errors */ }
});

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
