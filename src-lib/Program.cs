namespace LambdaCalculus;

public class Program
{
    public static async Task Main(string[] args)
    {
        Console.OutputEncoding = System.Text.Encoding.UTF8;
        Console.InputEncoding = System.Text.Encoding.UTF8;

        var interpreter = new Interpreter(logger: new());

        await interpreter.LoadFileIfExistsAsync("stdlib.lambda");
        await interpreter.EnsureRangeBuiltinsAsync();

        foreach (var filePath in args)
        {
            var result = await interpreter.LoadFileIfExistsAsync(filePath);
            if (result.StartsWith("File not found:"))
                Console.WriteLine(result);
        }
        await interpreter.EnsureRangeBuiltinsAsync();

        Logger.LogToConsole("");
        Logger.LogToConsole("Lambda Calculus Interpreter - Interactive Mode");
        Logger.LogToConsole("Type ':help' for a list of commands or ':exit' to quit");
        await interpreter.RunInteractiveLoopAsync();
    }
}