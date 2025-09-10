using LambdaCalculus;

var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

// Set a short cache for responses (2 seconds). No static files here, but add a general header.
app.Use(async (ctx, next) =>
{
    // Set headers before the response starts
    ctx.Response.Headers["Cache-Control"] = "public, max-age=2";
    ctx.Response.Headers["Expires"] = DateTime.UtcNow.AddSeconds(2).ToString("R");
    await next();
});

// Simple in-memory singleton interpreter instance
var interpreter = new Interpreter(logger: new());
await interpreter.LoadFileIfExistsAsync("stdlib.lambda");

// Health endpoints: new namespaced path plus legacy root for backward compatibility
app.MapGet("/api/health", () => Results.Ok(new { status = "ok" }));
app.MapGet("/health", () => Results.Ok(new { status = "ok", legacy = true }));

// Evaluate expression (query string: expr)
app.MapGet("/eval", async (string expr) =>
{
    var (exp, output) = await interpreter.ProcessInputAsync(expr);
    return Results.Ok(new { input = expr, output, normalized = exp?.ToString() });
});

// Load file (POST body: { "path": "..." })
app.MapPost("/load", async (LoadRequest req) =>
{
    if (string.IsNullOrWhiteSpace(req.Path)) return Results.BadRequest("Path required");
    var result = await interpreter.LoadFileIfExistsAsync(req.Path);
    return Results.Ok(new { message = result });
});

app.Run();

public record LoadRequest(string Path);
public partial class Program { }
