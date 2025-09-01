namespace LambdaCalculus;

/// <summary>
/// Central, descriptor-based native function registry to keep the definition of host provided
/// primitives declarative and easy to extend. Each native function describes:
///  - Primary name and optional aliases
///  - Arity constraints (min/max)
///  - Evaluation strategy for arguments (eager vs already CEK evaluated by caller)
///  - Implementation delegate producing an <see cref="Expr"/>
/// </summary>
internal static class NativeRegistry
{
    private record NativeDescriptor(
        string Name,
        string[] Aliases,
        int MinArity,
        int MaxArity,
        Func<Interpreter, string, List<Expr>, Dictionary<string, Expr>, Expr?> Impl,
        string Category,
        string? Doc = null);

    internal record NativeInfo(string Name, string[] Aliases, string Category, int MinArity, int MaxArity, string? Doc);

    private static readonly List<NativeDescriptor> _descriptors = new()
    {
        new(
            Name: "random",
            Aliases: [],
            MinArity: 1,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.IsRandom(name, args, env),
            Category: "math",
            Doc: "random n | random a b -> Church numeral within range (inclusive)"),
        new(
            Name: "alphaEq",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.AlphaEqNative(name, args, env),
            Category: "equality",
            Doc: "alphaEq a b -> Church boolean whether expressions are alpha-equivalent after normalization"),
        new(
            Name: "betaEq",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.BetaEqNative(name, args, env),
            Category: "equality",
            Doc: "betaEq a b -> normalize both then alphaEq"),
        new(
            Name: "hashEq",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.HashEqNative(name, args, env),
            Category: "equality",
            Doc: "hashEq a b -> compare canonical beta-normal form hashes (fast approximate equivalence)"),
        new(
            Name: "etaEq",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.EtaEqNative(name, args, env),
            Category: "equality",
            Doc: "etaEq a b -> beta-normalize then eta-reduce then alphaEq"),
        // Arithmetic (binary)
        ArithBinary("plus", ["+"], (a,b) => a + b, doc:"a + b"),
        ArithBinary("minus", ["-"], (a,b) => Math.Max(0, a - b), doc:"max(0,a-b)"),
        ArithBinary("mult", ["*"], (a,b) => a * b, doc:"a * b"),
        ArithBinary("div", ["/"], (a,b) => b==0?0:a/b, doc:"integer division (b==0 => 0)"),
        ArithBinary("mod", ["%"], (a,b) => b==0?0:a%b, doc:"a mod b (b==0 => 0)"),
        ArithBinary("exp", ["pow","^"], (a,b) => (int)Math.Pow(a,b), doc:"a^b"),
        ArithBinary("max", [], Math.Max),
        ArithBinary("min", [], Math.Min),
        // Arithmetic (unary)
        ArithUnary("succ", ["++","inc"], a => a + 1),
        ArithUnary("pred", ["--","decr"], a => Math.Max(0,a-1)),
        ArithUnary("square", [], a => a*a),
        ArithUnary("half", [], a => a/2),
        ArithUnary("sqrt", [], a => (int)Math.Sqrt(a)),
        // Comparison (binary)
        BoolBinary("lt", ["<"], (a,b)=> a < b),
        BoolBinary("leq", ["<="], (a,b)=> a <= b),
        BoolBinary("eq", ["=="], (a,b)=> a == b),
        BoolBinary("geq", [">="], (a,b)=> a >= b),
        BoolBinary("gt", [">"], (a,b)=> a > b),
        BoolBinary("neq", ["!="], (a,b)=> a != b),
        // Comparison (unary)
        BoolUnary("iszero", [], a => a == 0),
        BoolUnary("even", [], a => a % 2 == 0),
        BoolUnary("odd",  [], a => a % 2 != 0),
        // List / sequence primitives (structural pattern-match on cons/nil chains)
        new(
            Name: "length",
            Aliases: [],
            MinArity: 1,
            MaxArity: 1,
            Impl: (interp, name, args, env) => interp.LengthNative(args[0], env),
            Category: "list",
            Doc: "length list -> Church numeral length (pattern matches cons/nil)"),
        new(
            Name: "append",
            Aliases: ["concat"],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.AppendNative(args[0], args[1], env),
            Category: "list",
            Doc: "append a b -> concatenation of two lists (non-streaming)"),
        new(
            Name: "reverse",
            Aliases: [],
            MinArity: 1,
            MaxArity: 1,
            Impl: (interp, name, args, env) => interp.ReverseNative(args[0], env),
            Category: "list",
            Doc: "reverse list -> reversed list (non-streaming)"),
        new(
            Name: "map",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.MapNative(args[0], args[1], env),
            Category: "list",
            Doc: "map f list -> applies f to each element (eager)"),
        new(
            Name: "filter",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.FilterNative(args[0], args[1], env),
            Category: "list",
            Doc: "filter p list -> keeps elements where p elem is Church true"),
        new(
            Name: "take",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.TakeNative(args[0], args[1], env),
            Category: "list",
            Doc: "take n list -> first n elements (if n > length returns list)"),
        new(
            Name: "drop",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.DropNative(args[0], args[1], env),
            Category: "list",
            Doc: "drop n list -> list without first n elements"),
        new(
            Name: "any",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.AnyNative(args[0], args[1], env),
            Category: "list",
            Doc: "any p list -> Church boolean true if any element satisfies p"),
        new(
            Name: "all",
            Aliases: [],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.AllNative(args[0], args[1], env),
            Category: "list",
            Doc: "all p list -> Church boolean true if all elements satisfy p (false on empty? returns true)"),
        new(
            Name: "find",
            Aliases: ["findFirst"],
            MinArity: 2,
            MaxArity: 2,
            Impl: (interp, name, args, env) => interp.FindNative(args[0], args[1], env),
            Category: "list",
            Doc: "find p list -> maybe first element (just x | nothing)"),
        new(
            Name: "sum",
            Aliases: [],
            MinArity: 1,
            MaxArity: 1,
            Impl: (interp, name, args, env) => interp.SumNative(args[0], env),
            Category: "list",
            Doc: "sum list -> Church numeral sum of numeric elements"),
        new(
            Name: "product",
            Aliases: [],
            MinArity: 1,
            MaxArity: 1,
            Impl: (interp, name, args, env) => interp.ProductNative(args[0], env),
            Category: "list",
            Doc: "product list -> Church numeral product (empty => 1)"),
    };

    private static NativeDescriptor ArithBinary(string name, string[] aliases, Func<int,int,int> f, string category="math", string? doc=null)
        => new(name, aliases, 2, 2, (I, op, args, env) =>
        {
            if (!I.TryGetChurchInt(args[0], env, out var a) || !I.TryGetChurchInt(args[1], env, out var b)) return null;
            return I.MakeChurchNumeral(f(a,b));
        }, category, doc);

    private static NativeDescriptor ArithUnary(string name, string[] aliases, Func<int,int> f, string category="math", string? doc=null)
        => new(name, aliases, 1, 1, (I, op, args, env) =>
        {
            if (!I.TryGetChurchInt(args[0], env, out var a)) return null;
            return I.MakeChurchNumeral(f(a));
        }, category, doc);

    private static NativeDescriptor BoolBinary(string name, string[] aliases, Func<int,int,bool> f, string category="comparison", string? doc=null)
        => new(name, aliases, 2, 2, (I, op, args, env) =>
        {
            if (!I.TryGetChurchInt(args[0], env, out var a) || !I.TryGetChurchInt(args[1], env, out var b)) return null;
            return I.MakeChurchBoolean(f(a,b));
        }, category, doc);

    private static NativeDescriptor BoolUnary(string name, string[] aliases, Func<int,bool> f, string category="comparison", string? doc=null)
        => new(name, aliases, 1, 1, (I, op, args, env) =>
        {
            if (!I.TryGetChurchInt(args[0], env, out var a)) return null;
            return I.MakeChurchBoolean(f(a));
        }, category, doc);

    /// <summary>
    /// Registers all descriptors (primary names + aliases) into the interpreter instance.
    /// </summary>
    public static void RegisterAll(Interpreter interpreter)
    {
        foreach (var d in _descriptors)
        {
            RegisterDescriptor(interpreter, d);
        }
    }

    public static IEnumerable<NativeInfo> GetDescriptors()
        => _descriptors.Select(d => new NativeInfo(d.Name, d.Aliases, d.Category, d.MinArity, d.MaxArity, d.Doc));

    private static void RegisterDescriptor(Interpreter interpreter, NativeDescriptor descriptor)
    {
        // Wrapper matching existing delegate signature
        Expr? Impl(string name, List<Expr> args, Dictionary<string, Expr> env)
        {
            if (args.Count < descriptor.MinArity || args.Count > descriptor.MaxArity)
                return null; // Arity mismatch -> allow fallback to pure lambda form if user defined one
            return descriptor.Impl(interpreter, name, args, env);
        }

        interpreter.RegisterNativeFunction(descriptor.Name, Impl);
        foreach (var alias in descriptor.Aliases)
            interpreter.RegisterNativeFunction(alias, Impl);
    }
}