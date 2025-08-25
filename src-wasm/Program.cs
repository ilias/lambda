using LambdaCalculus;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;

namespace LambdaCalculus.Wasm;

public class Program
{
    public static async Task Main(string[] args)
    {
    var builder = WebAssemblyHostBuilder.CreateDefault(args);
    // Minimal host; add root component later if UI desired.
    var interpreter = new Interpreter(new Logger());
    await interpreter.LoadFileIfExistsAsync("stdlib.lambda");
    await interpreter.EnsureRangeBuiltinsAsync();
    await Console.Out.WriteLineAsync("Lambda Calculus Interpreter (WASM) initialized");
    await builder.Build().RunAsync();
    }
}