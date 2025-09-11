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

// Lint-only endpoint: returns parse diagnostics without evaluating or mutating env
app.MapGet("/api/lint", (string expr) =>
{
    try
    {
        // Try tokenize + parse to collect syntax errors
        var tokens = interpreter._parser.Tokenize(expr);
        interpreter._parser.ParseAll(expr);
        return Results.Ok(new { ok = true, diagnostics = Array.Empty<object>() });
    }
    catch (LambdaCalculus.ParseException pe)
    {
        // Map absolute position to 1-based line/column
        int pos = Math.Max(0, pe.Position);
        int line = 1, col = 1;
        for (int i = 0; i < pos && i < expr.Length; i++)
        {
            if (expr[i] == '\n') { line++; col = 1; }
            else col++;
        }
        var diag = new
        {
            message = pe.Message,
            errorType = pe.ErrorType.ToString(),
            position = pe.Position,
            range = new { startLineNumber = line, startColumn = col, endLineNumber = line, endColumn = Math.Max(col, 1) + 1 }
        };
        return Results.Ok(new { ok = false, diagnostics = new[] { diag } });
    }
    catch (Exception ex)
    {
        var diag = new
        {
            message = ex.Message,
            errorType = "GeneralError",
            position = 0,
            range = new { startLineNumber = 1, startColumn = 1, endLineNumber = 1, endColumn = 1 }
        };
        return Results.Ok(new { ok = false, diagnostics = new[] { diag } });
    }
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
