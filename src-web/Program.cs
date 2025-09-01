using LambdaCalculus;

var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

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
