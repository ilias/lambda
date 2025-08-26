# Lambda Calculus Interpreter

A high-performance lambda calculus interpreter written in C# featuring lazy evaluation, comprehensive standard library, infix operators, macros, and native arithmetic optimizations.

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
- [Core Language](#core-language)
- [Formal Grammar](#formal-grammar)
- [Advanced Syntax Features](#advanced-syntax-features)
- [Interactive Commands](#interactive-commands)
- [Command / Expression Chaining](#command--expression-chaining)
- [Standard Library](#standard-library)
- [Infix Operators](#infix-operators)
- [Macro System](#macro-system)
- [Examples (Extended)](#examples-extended)
- [Advanced Usage](#advanced-usage)
- [Performance Features](#performance-features)
- [Building and Running](#building-and-running)
    - [Web UI & Streaming Logs](#web-ui--streaming-logs)
    - [Docker (Web UI)](#docker-web-ui)
    - [Streaming Modes (Comparison)](#streaming-modes-comparison)
- [Range Syntax Extensions](#range-syntax-extensions)
- [Parser Errors & Diagnostics](#parser-errors--diagnostics)
- [Unary Minus / Negative Literals](#unary-minus--negative-literals)
- [Either Type (Error Handling)](#either-type-error-handling)
- [Pretty Printing](#pretty-printing)
- [Step Tracing & Debugging](#step-tracing--debugging)
- [Performance Cookbook](#performance-cookbook)
- [Embedding & Programmatic API](#embedding--programmatic-api)
- [Multi-user & Deployment Strategies](#multi-user--deployment-strategies)

## Features

### Core Language Features

- **Pure Lambda Calculus**: Supports variables, lambda abstractions, and function application
- **Church Numerals**: Built-in support for integers using Church encoding
- **Lazy Evaluation**: Efficient lazy evaluation with thunk caching (can be toggled to eager)
- **Y Combinator**: Built-in support for recursion via the Y combinator
- **Multi-line Input**: Intelligent multi-line expression support with auto-completion detection
- **Top-level Sequencing**: Semicolon (`;`) support to evaluate multiple expressions sequentially at the REPL or in files

### Advanced Features

- **Infix Operators**: Define custom infix operators with precedence and associativity
- **Macro System**: Powerful pattern-driven macro system (multi-clause, guards, variadic/rest arguments, precedence & shadowing)
- **Native Arithmetic & User Primitives**: Optional native arithmetic optimizations for Church numerals, plus support for user-defined native primitives (see below)
- **Pretty Printing**: Automatic formatting of Church numerals and lists
- **Comprehensive Standard Library**: Over 200 predefined functions and utilities
- **General & Stepped Ranges**: Rich list range syntax `[a .. b]`, `[a, b .. c]` with lazy dynamic expansion

### Performance Optimizations

- **Memoization**: Multiple caching layers for substitution, evaluation, and free variables
- **Expression Interning**: Memory-efficient expression representation
- **Stack-based Evaluation**: CEK (Control, Environment, Kontinuation) machine for efficient evaluation
- **Thunk Forcing**: Lazy evaluation with intelligent thunk management

### Recent UI / Tooling Updates (2025-08)

Enhancements to the Web UI (`src-webui`):

- Multi-Output Tabs: Separate sessions, independent logs and filters.
- Log Filters: Toggle Macro, Test, Time, Step, Eval, Norm, Name, Processing categories.
- Enhanced Search: Floating navigator, keyboard shortcuts (`/`, `F3`, `Shift+F3`, `n`, `N`, `Esc`).
- Normalized Output Handling: `Norm:` lines only when distinct; blank spacer lines emitted only when normalization appears.
- Local File Loader: Client-side `.lambda` loading with progress bar.
- Streaming Transports: Buffered vs SSE vs WebSocket switchable at runtime.
- Cache Busting: Timestamped resource fetches + cache/service worker clearing.
- Persistent History: Up/Down arrow navigation with `localStorage` retention.
- Alpha Equivalence Logging: Consistent `Alpha left/right/passed|failed` lines; visibility controlled purely by filters.
- Wrapped Output: Long lines wrap; horizontal scroll removed.

Tip: Extend filtering or log classification via `classify()` and `FILTER_CONFIG` in `wwwroot/index.html`.

## Getting Started

### Project Layout (Library + CLI + Web)

The repository now consists of three projects:

| Project | Type | Path | Purpose |
|---------|------|------|---------|
| `lambda-cek` | Class Library (multi-target `net8.0; net9.0`) | `src/` | Core interpreter engine (parsing, evaluation, macros, etc.) |
| `lambda-cek.cli` | Console App (`net8.0`) | `src-cli/` | Interactive REPL and file runner built on the library |
| `lambda-cek.web` | Minimal ASP.NET Core Web API (`net8.0`) | `src-web/` | Simple HTTP interface to evaluate expressions & load files |
| `lambda-cek.webui` | ASP.NET Core static + API (`net8.0`) | `src-webui/` | Browser UI (HTML/JS) plus namespaced API (`/api/*`) |

### Build Everything

```bash
dotnet build
```

### Run CLI (REPL)

```bash
dotnet run --project src-cli -- [optional .lambda files]
```

Examples:

```bash
dotnet run --project src-cli
dotnet run --project src-cli -- examples.lambda tests.lambda
```

### Run Web API

```bash
dotnet run --project src-web
```

Default endpoints (no auth, for local/dev use):

| Method | Route | Description | Example |
|--------|-------|-------------|---------|
| GET | `/api/health` | Liveness check (new) | `curl http://localhost:5000/api/health` |
| GET | `/health` | (Legacy) liveness check | `curl http://localhost:5000/health` |
| GET | `/eval?expr=EXPR` | Evaluate a single expression | `curl "http://localhost:5000/eval?expr=succ%2041"` |
| POST | `/load` (JSON `{ "path": "file.lambda" }`) | Load a file into the global environment | `curl -X POST -H "Content-Type: application/json" -d '{"path":"stdlib.lambda"}' http://localhost:5000/load` |

Returns JSON like:

```json
{ "input": "succ 1", "output": "2", "normalized": "2" }
```

### Use as a Library

Reference `src/lambda-cek.csproj` from another project and instantiate:

```csharp
var interp = new LambdaCalculus.Interpreter(logger: new LambdaCalculus.Logger());
await interp.LoadFileIfExistsAsync("stdlib.lambda");
var (expr, result) = await interp.ProcessInputAsync("succ 41");
Console.WriteLine(result); // 42
```

### Notes

- Warnings (XML docs) are expected; enable / suppress as needed.
- The web API keeps a single in-memory interpreter instance; scale-out would need state strategy (e.g., per-session or stateless evaluation model).
- For production hosting harden the endpoints (validation, timeouts, resource limits).
- The Web UI project exposes `/api/eval`, `/api/load`, and `/api/health` (preferred liveness endpoint). It also serves a single-page interface from `wwwroot`.

### Run Web UI

```bash
dotnet run --project src-webui
```

Then open <http://localhost:5000> (or the shown port) in your browser. Health status appears in the footer.

## Web UI & Streaming Logs

The Web UI (`src-webui`) provides a browser-based REPL with real-time log streaming and colored output mirroring the CLI.

### Transports

Two streaming transports are available for incremental log delivery:

| Transport | Endpoint | Notes |
|-----------|----------|-------|
| Server-Sent Events (SSE) | `/api/stream` | Default; simple, auto‑reconnect logic implemented |
| WebSocket | `/ws` | Optional; toggle with the UI button ("WS: On/Off") |

Toggle streaming via the "Streaming" button. When disabled, evaluation responses include a buffered copy of the logs (`logs` array) in the JSON payload (gathered from the interpreter's in‑memory log buffer). When enabled, logs are pushed line-by-line; the final JSON response only supplies the canonical result(s).

### Progress Feedback

While loading `.lambda` files the interpreter emits progress pseudo‑log lines in the form:

```text
PROGRESS::<percent>
```

The UI converts these into a progress bar; when 100% is reached the bar auto-hides after a short delay. A final timing / summary line is also streamed (e.g. `Loaded stdlib.lambda (N lines in X ms)`).

### Colored Log Classification

Incoming log lines are classified on the client (prefix / pattern heuristics) and assigned CSS classes:

| Class | Meaning (examples) |
|-------|--------------------|
| `log-error` | Error diagnostics (`Error:` …) |
| `log-result` | Result marker lines starting with `->` |
| `log-step` | Step-by-step evaluation traces (when `:step on`) |
| `log-time` / `log-name` | Timing / named expression metadata |
| `log-eval` | Evaluation summaries (`Eval:`) |
| `log-macro` | Macro expansion info |
| `log-test`, `log-test-pass`, `log-test-fail` | Structural test counters / outcomes |
| `log-command` | Echoed colon commands (lines beginning with `:`) |
| `log-loading` | File loading lines (contain `Loading`) |
| `log-fileline` / `log-fileresult` | Per-line file load traces / outcomes |
| `log-progress` | Progress pseudo-lines |

The classification is intentionally lightweight; adding new patterns only requires editing `classify()` inside `wwwroot/index.html`.

### Log Retention

To prevent the browser DOM from growing unbounded during long step traces, the UI caps the number of displayed log lines (default 2000). Older nodes are trimmed once the cap is exceeded. Adjust `MAX_LOG_LINES` in `index.html` if needed.

### Typical Workflow

1. Start the UI: `dotnet run --project src-webui`
2. (Optional) Click `Streaming: On` (or append `#stream` to the URL for auto-enable)
3. (Optional) Enable WebSocket transport (`WS: On`) if desired
4. Enter expressions or colon commands (e.g. `:stats`, `succ 41`, `let x = 5 in x*x`)
5. Load additional files via the right-hand panel (`stdlib.lambda` is preloaded if found)

### API Summary (Web UI Namespace)

| Method | Route | Purpose |
|--------|-------|---------|
| GET | `/api/eval?expr=...` | Evaluate a single expression (returns JSON with `output`, `normalized`, and `logs` when not streaming) |
| POST | `/api/load` (JSON `{ path }`) | Load a `.lambda` file; returns `message` + `logs` |
| GET | `/api/stream` | SSE endpoint for real-time log push (one `data:` line per log) |
| GET | `/api/health` | Health/liveness check |
| WS | `/ws` | WebSocket log stream (one text frame per log line) |

The interpreter instance is shared; stateful definitions/macros persist across requests and streams. For multi-user deployment, isolate interpreters (per session) or introduce workspace scoping.

### Disabling Streaming / Fallback

If streaming is off, interactive operations rely solely on the buffered log snapshot captured between `Logger.ClearBuffer()` and evaluation completion. This ensures feature parity for environments where SSE / WebSockets are blocked.

### Extensibility

You can instrument additional events by invoking `Logger.Log("...")` within interpreter code paths—those lines will appear uniformly across CLI and Web transports. Keep emitted lines single-line (newlines are flattened in SSE) for predictable streaming.

### Streaming Modes (Comparison)

| Mode | Enable | Transport | Log Delivery | Response JSON | Best For |
|------|--------|-----------|--------------|---------------|---------|
| Buffered | Streaming Off | none | After completion (batched) | `{ output, normalized, logs[] }` | Simplicity, copy full log |
| SSE | Streaming On | `/api/stream` | Incremental (auto-reconnect) | `{ output, normalized }` | Progressive feedback, long loads |
| WebSocket | Streaming On + WS On | `/ws` | Incremental (manual reconnect logic) | `{ output, normalized }` | Future interactive extensions |

Notes:
 
1. Toggling streaming clears the UI log to visually separate sessions.
2. SSE auto-reconnect delay is capped; repeated disconnects are surfaced with a status banner.
3. WebSocket mode is optional; fallback to SSE if handshake fails.
4. All modes share the same underlying `Logger`; difference is purely transport serialization.

## Docker (Web UI)

Container support is provided for the Web UI project (`src-webui`). The Dockerfile builds only the `net8.0` target (temporarily rewriting the multi-target library to avoid requiring a .NET 9 SDK inside the image).

### Build Image

```bash
docker build -t lambda-cek-webui -f src-webui/Dockerfile .
```

### Run Container

```bash
docker run --rm -p 8080:8080 --name lambda-cek lambda-cek-webui
```

Then open <http://localhost:8080>.

### Bind Mount Files

Mount the repo to allow loading `stdlib.lambda` or custom files (PowerShell / CMD on Windows, replace `%CD%` appropriately for Linux/macOS):

```bash
docker run --rm -p 8080:8080 -v %CD%:/data:ro --name lambda-cek lambda-cek-webui
```

The app looks for `stdlib.lambda` in the working directory, its parent, or the base directory. Adjust with `-w` or copy files into the image for immutable deployments.

### Change Port

```bash
docker run --rm -e ASPNETCORE_URLS=http://0.0.0.0:5005 -p 5005:5005 lambda-cek-webui
```

### Multi-Arch Build (Optional)

```bash
docker buildx build --platform linux/amd64,linux/arm64 -t yourrepo/lambda-cek-webui:latest .
```

### Updating for .NET 9

Remove the `sed` line in the Dockerfile and use a .NET SDK image that supports `net9.0`. Keep multi-targeting only if needed for consumers.

### Security Notes

- No authentication / sandboxing by default.
- Long or CPU-heavy expressions can monopolize the single interpreter instance.
- Add resource limits (CPU, memory) and reverse proxy restrictions for shared environments.


## User-Defined Native Primitives

You can extend the interpreter with your own native (host language) primitives. This is useful for adding custom arithmetic, logic, or interop functions.

### Registering a Native Primitive (C# Example)

In your host C# code:

```csharp
interpreter.RegisterNativeFunction("inc", (args, env) =>
{
    if (args.Count == 1 && interpreter.TryGetChurchInt(args[0], env, out var n))
        return interpreter.MakeChurchNumeral(n + 1);
    return null;
});
```

This makes `inc` available as a function in your lambda calculus code:

```lambda
inc 2   # returns 3 (as a Church numeral)
```

You can register, override, or remove any primitive at runtime. All user-defined primitives are listed in the environment display (see `:env` command).

### Running the Interpreter

```bash
# Start interactive mode
./lambda-cek

# Load files at startup
./lambda-cek file1.lambda file2.lambda

# The standard library is automatically loaded from stdlib.lambda
```

### Basic Syntax

```lambda
# Variables and application
x                        # Variable
f x                      # Function application

# Lambda abstractions
λx.x                     # Identity function
\x.x                     # Alternative syntax
x -> x                   # Arrow function syntax (same as λx.x)

# Placeholder variables (underscore for ignored parameters)
_ -> 42                  # Constant function, parameter ignored → λ_1.42
(x, _, _ -> x) 42 9 8    # Extract first of three arguments → 42
_ + _ -> mult _ _         # Multiple underscores become unique variables
map (_ -> 0) [1, 2, 3]   # Zero out all elements → [0, 0, 0]

# Multi-parameter functions
λx y.x                   # Same as λx.λy.x
x, y -> x + y            # Multi-parameter arrow function → λx.λy.x + y

# Church numerals
0, 1, 2, 42              # Integer literals → λf.λx.x, λf.λx.f x, λf.λx.f (f x), etc.

# Let expressions (syntactic sugar for function application)
let id = x -> x in id 42                    # → (λid.id 42) (λx.x)
let add = x, y -> x + y in add 3 4          # → (λadd.add 3 4) (λx.λy.x + y)

# Multiple let bindings
let x = 3, y = 4 in x * y                   # → (λx.λy.x * y) 3 4

# Recursive definitions (uses Y combinator)
let rec factorial = n -> if (iszero n) 1 (mult n (factorial (pred n))) in factorial 5
# → (λfactorial.factorial 5) (Y (λfactorial.λn.if (iszero n) 1 (mult n (factorial (pred n)))))

# Lists (syntactic sugar for cons/nil structures)
[1, 2, 3]               # → cons 1 (cons 2 (cons 3 nil))
[]                      # → nil
[1 .. 5]                # → cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))
[10 .. 5]               # → cons 10 (cons 9 (cons 8 (cons 7 (cons 6 (cons 5 nil)))))
[-3 .. 3]               # Negative & positive literal range
[1, 3 .. 11]            # Stepped range (step = 2) → [1, 3, 5, 7, 9, 11]
[10, 7 .. -2]           # Descending stepped range (step = -3) → [10, 7, 4, 1, -2]

# General / dynamic ranges (non-literal endpoints are desugared, not expanded eagerly)
[f x .. g y]            # Desugars to (range (f x) (g y)) and is produced lazily
[a, a+2 .. b]           # Desugars to (range2 a (a+2) b) when any part is non-literal

# Built-in operators
5 |> succ |> mult 2     # Pipeline operator: left-to-right data flow → 12
(mult 2) . succ         # Composition operator: right-to-left function building
3 + 4 * 5               # Infix arithmetic (when operators are defined) → 23

# Comments
# This is a comment

# Semicolon sequencing (top-level only)
expr1; expr2; expr3     # Evaluate expr1, then expr2, then expr3; final result shown
let x = 5 in x; succ 10 # Two separate evaluations

# Notes:
# - Semicolons are only recognized at the top level (REPL root or file root)
# - A semicolon inside a list, lambda, let, etc. triggers a parse error (UnexpectedSemicolon)
```

### Advanced Syntax Features

#### Underscore Placeholder Variables

The underscore (`_`) serves as a placeholder for ignored or unused parameters in lambda expressions:

```lambda
# Single underscore for ignored parameters
const42 = _ -> 42                    # Always returns 42, ignores input
const42 100                          # → 42

# Multiple underscores become unique variables
first = (x, _, _) -> x               # Extract first of three arguments
first 1 2 3                          # → 1

second = (_, y, _) -> y              # Extract second of three arguments
second 1 2 3                         # → 2

# Underscores in function bodies refer to parameters
swapArgs = (_, _) -> _ _             # Each _ refers to a unique parameter position
# Equivalent to: (x, y) -> y x

# Practical examples with higher-order functions
map (_ -> 0) [1, 2, 3, 4]           # Zero out all elements → [0, 0, 0, 0]
filter (_ -> true) [1, 2, 3]        # Keep all elements → [1, 2, 3]
foldl (_ acc -> acc) 0 [1, 2, 3]    # Ignore values, keep accumulator → 0

# Complex expressions with mixed parameters
transform = (x, _, z) -> x + z       # Use first and third, ignore second
transform 10 999 5                   # → 15

# Underscore with operators
addBoth = _ + _ -> mult _ _           # Each _ is a unique parameter
# Equivalent to: (x, y) -> mult (x + y) (x + y)
```

**Key Features:**

- Each `_` in a lambda parameter list becomes a unique, auto-generated variable
- Underscores in the function body refer back to these auto-generated parameters
- Useful for partial application patterns and when some parameters are irrelevant
- Improves code readability by explicitly showing which parameters are ignored

## Interactive Commands

The interpreter provides numerous commands for managing your session:

### Environment Management

```shell
:clear                   # Clear everything (env + macros + infix ops + stats + caches)
:clear defs              # Clear only definitions (env); keep macros & infix ops
:clear macros            # Clear only macro clauses
:clear ops               # Clear custom infix operators (restores defaults |>, .)
:clear cache             # Clear all memoization / analysis caches
:clear all               # Same as bare :clear
:env                     # Show environment (defs, macros, infix, natives)
:env defs                # Show only definitions
:env macros              # Show only macros
:env infix               # Show only infix operators
:env native              # Show native primitives
:load <file>             # Load definitions from file
:save <file>             # Save current environment to file
```

### Evaluation Control

```shell
:lazy on|off             # Toggle lazy/eager evaluation
:step on|off             # Toggle step-by-step evaluation
:depth [n]               # Set/show recursion depth limit (10-10000)
:native on|off           # Toggle native arithmetic optimizations
:pretty on|off           # Toggle pretty printing
```

### Debugging and Performance

```shell
:stats                   # Show detailed performance statistics
:test clear              # Reset structural equality test counters
:test result             # Show structural equality test counters (calls/successes)
:clear cache             # Clear all caches (memoization)
:log <file|off>          # Enable/disable logging to file
```

### Language Extensions

```shell
:infix <op> <prec> <assoc>  # Define infix operator
:macro (pattern) => body    # Define macro
:env macros                # List macro clauses only (shortcut for filtering)
```

### Help and Information

```shell
:help                    # Show comprehensive help (includes multi-line input usage)
:native show             # Show all native arithmetic functions
:env [defs|macros|infix|native|all]  # Show environment (optionally filtered)
```

## Command / Expression Chaining

You can place multiple commands and/or expressions on a single line (or in a file) separated by top‑level semicolons. This works uniformly for colon commands (e.g. `:load`, `:macro`, `:infix`) and ordinary expressions.

### Examples

```shell
# Define a macro then immediately use it
:macro (inc $x) => (succ $x); inc 5            # → 6

# Load a file then run an expression
:load stdlib.lambda; plus 2 3                  # (loads stdlib, then evaluates) → 5

# Chain several expressions (only final result is printed; earlier ones still evaluate)
let x = 10 in succ x; let y = 2 in mult y 5    # → 10

# Mix multiple commands and expressions
:macro (sq $x) => (mult $x $x); :env macros; sq 7  # Lists macros, then evaluates → 49
```

### Rules

- Semicolons are recognized only at the top level (i.e. not inside parentheses, brackets, lambdas, lets, lists, or macro bodies).
- Empty segments (e.g. stray trailing `;`) are ignored.
- `:exit` / `:quit` stop further processing of subsequent segments on the same line.
- Each segment is processed in left‑to‑right order; environment changes (definitions, macros, infix ops) take effect for later segments in the same line.
- Errors in a segment abort the remaining segments on that line.

### Practical Uses

- Rapid prototyping: define + test in one line.
- Batch loading: `:load a.lambda; :load b.lambda; :stats`.
- Macro refinement: redeclare a macro and immediately inspect with `:env macros`.

This feature streamlines interactive workflows by reducing the number of round trips required to iterate on definitions and tests.

## Core Language

### Lambda Abstractions

```lambda
# Basic lambda
id = λx.x
id = \x.x                # Alternative syntax (backslash)
id = x -> x              # Arrow function syntax → λx.x

# Multi-parameter functions
add = λx y.x + y                    # Standard lambda calculus
add = x, y -> x + y                 # Arrow syntax → λx.λy.x + y

# Higher-order functions
twice = f -> x -> f (f x)           # → λf.λx.f (f x)
compose = f -> g -> x -> f (g x)    # → λf.λg.λx.f (g x)
```

## Formal Grammar

This section specifies the concrete grammar accepted by the parser after the Pratt refactor. Grammar is written in an EBNF‑style notation; terminals appear in single quotes; parentheses group; `*` = 0+ repetition, `+` = 1+ repetition, `?` = optional, `|` = alternation.

Precedence (loosest → tightest):

```text
let  <  arrow (paramList '->')  <  infix operators (by declared precedence number)  <  application  <  atom
```

Associativity:

- Let: right (body extends to the rightmost expression)
- Arrow (`->`): right (parameters associate to the right, `x, y, z -> e` = `x -> (y -> (z -> e))`)
- Infix: per definition (`:infix <op> <prec> left|right`); higher precedence number binds tighter
- Application: left (juxtaposition chains left‑to‑right)

Lexical elements:

```ebnf
Identifier    ::= Letter (Letter | Digit | '_' | '?')*
Integer       ::= Digit+            # Parsed into Church numeral
Underscore    ::= '_'               # Placeholder parameter (fresh name generated)
Comment       ::= '#' <until end of line>
Whitespace    ::= (' ' | '\t' | '\r' | '\n')+
Arrow         ::= '->'
LambdaIntro   ::= 'λ' | '\\'
Ellipsis      ::= '...'
```

Top level & segmentation:

```ebnf
Program       ::= Segment (';' Segment)*                      # Semicolons only at top level
Segment       ::= (Command | Definition | Expression)?        # Empty segments ignored
Command       ::= ':' CommandName CommandArgs?
CommandName   ::= Identifier | 'infix' | 'macro' | 'macros' | 'load' | 'clear' | ... (see Interactive Commands)
Definition    ::= Identifier '=' Expression                   # Eagerly evaluated & stored
```

Expressions (ordered by parsing precedence):

```ebnf
Expression    ::= LetExpr | ArrowExpr | InfixExpr

LetExpr       ::= 'let' ('rec')? LetBinding (',' LetBinding)* 'in' Expression
LetBinding    ::= Identifier '=' Expression
               | Identifier ParamList '->' Expression         # Sugar: params desugar to nested lambdas

ArrowExpr     ::= ParamList '->' Expression                   # Recognized only if followed by '->'
ParamList     ::= Param (',' Param)* | '(' Param (',' Param)* ')'
Param         ::= Identifier | Underscore

InfixExpr     ::= Application (InfixOp Application)*          # Pratt: precedence & associativity driven
InfixOp       ::= OperatorSymbol                              # Previously defined via :infix

Application   ::= Atom+                                       # Left associative
Atom          ::= Integer
               | Identifier
               | Lambda
               | List
               | '(' Expression ')'
               | MacroInvocation

Lambda        ::= LambdaIntro Param+ '.' Expression           # λx y z.body → λx.λy.λz.body
MacroInvocation ::= Identifier Arg*                           # After expansion behaves as ordinary expr
Arg           ::= Atom | '(' Expression ')'
```

Lists & ranges:

```ebnf
List          ::= '[' ListBody? ']'
ListBody      ::= Elements | RangeSpec | SteppedRangeSpec
Elements      ::= Expression (',' Expression)*
RangeSpec     ::= Expression '..' Expression                  # Literal endpoints → expanded; else desugars (range a b)
SteppedRangeSpec ::= Expression ',' Expression '..' Expression # Desugars (range2 a b c) if non‑literal
```

Notes:

1. Ranges expand eagerly only when all endpoints (and middle for stepped) are integer literals (supports negative forms `-3`).
2. A stepped range with zero step yields a singleton list.
3. Non‑literal endpoints produce a desugaring: `[a .. b]` → `(range a b)`, `[a, b .. c]` → `(range2 a b c)`.

Macros & directives:

```ebnf
MacroDef      ::= ':macro' '(' Pattern ')' GuardOpt '=>' Expansion
GuardOpt      ::= ('when' '(' Expression ')')?
Pattern       ::= PatternPart+                               # $var, symbols, rest ($xs ... at tail)
RestPattern   ::= '$' Identifier Ellipsis
Expansion     ::= Expression                                  # Parsed pre‑expansion then substituted
InfixDecl     ::= ':infix' OperatorSymbol Integer ('left' | 'right')
OperatorSymbol ::= NonAlnumSymbol+                            # e.g. +, *, ^, &&, |> , . , <=
```

Placeholders:

```text
Every '_' in a ParamList becomes a fresh, unique variable (e.g., _placeholder1) and may be referenced positionally in the body with the same `_` spelling.
```

Desugarings (informal):

```text
let x = A in B                ≡ (λx.B) A
let x = a, y = b in B         ≡ (λx.λy.B) a b
let rec f = E in B            ≡ (λf.B) (Y (λf.E))
x, y, z -> R                  ≡ x -> (y -> (z -> R)) ≡ λx.λy.λz.R
[a, b, c]                     ≡ cons a (cons b (cons c nil))
a |> f                        ≡ f a        (left‑to‑right pipeline)
f . g                         ≡ λx.f (g x) (composition)
```

Error Handling Summary (parser): `UnexpectedToken`, `MissingLetEquals`, `UnexpectedArrow`, `UnexpectedComma`, `EmptyExprList`, `UnexpectedSemicolon`, `IllegalAssignment`, `UnexpectedDot`, `UnterminatedList`, `MacroPatternError`, etc. See "Parser Errors & Diagnostics" for details.

For a condensed in‑REPL quick reference, run `:help`. The help view omits some extended macro and range details for brevity.

---

Rationale: The grammar is optimized for clarity over minimality. Pratt parsing handles `InfixExpr` with dynamic precedence tables populated at runtime via `:infix` declarations; thus `InfixOp` is not a fixed token set.

---

### Parser Errors & Diagnostics

This section enumerates the most common parser errors along with their meaning and likely remedies.

| Error | Meaning / Trigger | Typical Fix |
|-------|-------------------|-------------|
| `UnexpectedToken` | Token doesn't fit current production | Check for missing delimiters or stray characters |
| `MissingLetEquals` | `let x ? y` missing `=` | Insert `=` between binding name and expression |
| `UnexpectedArrow` | Misplaced `->` (e.g., outside lambda/guard) | Ensure correct lambda syntax `λx.y` or guard form |
| `UnexpectedComma` | Extra comma in lists, params, or ranges | Remove trailing or double commas |
| `EmptyExprList` | `[]` where at least one element required (contextual) | Provide an element or remove brackets |
| `UnexpectedSemicolon` | Stray `;` not used in this language | Remove `;` |
| `IllegalAssignment` | Attempt to assign to non‑identifier | Use identifier on left side of binding |
| `UnexpectedDot` | Misplaced `..` or `.` in non‑range context | Correct range syntax or remove dot |
| `UnterminatedList` | Missing closing `]` | Add `]` |
| `MacroPatternError` | Malformed macro pattern / guard | Adjust pattern syntax / guard expression |

Diagnostics Strategy:

1. The parser accumulates context to produce precise span highlights (future: structured JSON diagnostics output for tooling).
2. Multi‑line input preserves original indentation; caret alignment in UI mirrors source columns.
3. Planned: colorized squiggles in the Web UI and a `:errors` command to reprint the last parse failure set.

### Unary Minus / Negative Literals

Negative integers are parsed as a unified literal token rather than a prefix application of `neg` to simplify downstream evaluation and enable eager list range expansion with negative endpoints.

Rules:

1. `-0` normalizes to `0`.
2. A minus immediately followed by digits with no intervening space becomes a literal (e.g. `-42`).
3. A space before digits (`- 5`) is parsed as the binary subtraction operator (if defined) or a future infix candidate.
4. In ranges, `[-3 .. 3]` expands eagerly; stepped forms like `[-1,1 .. 5]` compute step = 2.
5. Pretty‑printer preserves original negative literal tokens without inserting extra parentheses.

Edge Cases:

```lambda
[-1]          # singleton list with negative literal
[ -1 ]        # same (whitespace ignored)
(-1)          # still a literal; parentheses preserved only if required by surrounding infix precedence
map (plus -1) [0,1,2]  # partial application with negative literal argument
```

Rationale: Treating negative numbers as atomic avoids an explosion of parentheses in normalized forms and keeps macro pattern matching simpler (patterns can match `$n` against negative numerals directly).


### Church Numerals

Church numerals represent natural numbers as functions:

```lambda
# Church numeral n = λf.λx.f^n(x)
0                        # λf.λx.x
1                        # λf.λx.f x
2                        # λf.λx.f (f x)

# Arithmetic operations
plus 2 3                 # 5
mult 4 5                 # 20
exp 2 3                  # 8 (2^3)
pred 5                   # 4 (predecessor)
```

### Lists

Lists are implemented as right-folded structures:

```lambda
# List construction
[1, 2, 3]               # cons 1 (cons 2 (cons 3 nil))
[]                      # nil (empty list)
[1 .. 5]                # [1, 2, 3, 4, 5]
[10 .. 5]               # [10, 9, 8, 7, 6, 5] (descending)

#### Range Syntax Extensions
```

#### Range Syntax Extensions

The interpreter supports an expressive Haskell‑style range family:

```lambda
[a .. b]        # Inclusive range; expands immediately if a & b are integer literals
[a, b .. c]     # Stepped range; step = b - a; expands immediately if all literals
[-5 .. 5]       # Negative literals allowed
[10, 7 .. -2]   # Descending stepped range using negative step
[f x .. g y]    # Dynamic endpoints → desugars to (range (f x) (g y)) lazily
[a, a+2 .. b]   # Dynamic stepped → desugars to (range2 a (a+2) b)
```

Expansion Rules:

1. If every endpoint (and the second element for stepped ranges) is a literal integer, the list is eagerly expanded at parse time.
2. Otherwise it desugars to one of the built-ins:
    - `[start .. end]` → `(range start end)`
    - `[a, b .. c]` → `(range2 a b c)` (step = b - a)
3. A zero step (e.g. `[5,5 .. 10]`) yields a singleton `[5]`.
4. Stepped progression stops before crossing the target bound (inclusive if exactly hits it).

These built-ins are injected automatically if not already defined:

```lambda
range a b         # (Built-in injected) generates numbers from a to b (ascending or descending) lazily
range2 a b c      # Stepped; b supplies a+step; works both directions lazily
```

Practical examples:

```lambda
map (mult 2) [1, 3 .. 11]          # [2, 6, 10, 14, 18, 22]
sum [10, 7 .. -5]                  # Handles descending & negative endpoints
take 5 [f n .. g n]               # Works with dynamic expressions lazily
```

Error Cases:

- Missing second element in a stepped form (e.g. `[a, .. b]`) raises a parse error.
- Extra commas or malformed patterns (e.g. `[a, b, c .. d]`) are rejected.
- Double dots in non-range contexts raise `UnexpectedDot`.

List operations:

```lambda
head [1, 2, 3]          # 1
tail [1, 2, 3]          # [2, 3]
length [1, 2, 3]        # 3
append [1, 2] [3, 4]    # [1, 2, 3, 4]
```

#### Multi-line Input

The interpreter supports intelligent multi-line input:

```lambda
# Automatic continuation for incomplete expressions
lambda> let factorial = Y (λf n.
......> [2]     if (iszero n) 1 
......> [3]         (mult n (f (pred n))))
......> [4] in factorial 5

# Manual continuation with backslash
lambda> let result = very_long_expression \
......> [2] that_continues_here in result

# Multi-line commands while editing
:cancel                            # Discard current input
:show                              # Display current buffer
:abort                             # Same as :cancel
```

## Standard Library

The standard library (`stdlib.lambda`) provides over 200 functions organized into categories:

### 1. Core Combinators

```lambda
# Basic combinators
I = λx.x                 # Identity
K = λx y.x               # Constant
S = λx y z.x z (y z)     # S combinator
B = λx y z.x (y z)       # Composition
C = λx y z.x z y         # Flip

# Y combinator is built-in for recursion
fact = Y (λf n.if (iszero n) 1 (mult n (f (pred n))))
```

### 2. Boolean Logic

```lambda
# Boolean values and operations
true, false             # Church booleans
not, and, or, xor       # Logical operations
if                      # Conditional (λp a b.p a b)

# Examples
and true false          # false
or true false           # true
if (gt 5 3) "yes" "no"  # "yes"
```

### 3. Arithmetic Operations

#### Basic Arithmetic

```lambda
# Church numeral arithmetic
plus 3 4                # 7
minus 10 3              # 7
mult 6 7                # 42
div 15 3                # 5
mod 17 5                # 2
exp 2 8                 # 256

# Additional operations
succ 5                  # 6 (successor)
pred 5                  # 4 (predecessor)
iszero 0                # true
double 7                # 14
square 8                # 64
```

#### Comparisons

```lambda
eq 5 5                  # true
neq 3 7                 # true
lt 3 8                  # true
leq 5 5                 # true
gt 8 3                  # true
geq 5 5                 # true
max 8 12                # 12
min 5 9                 # 5
```

#### Advanced Math

```lambda
# Mathematical functions
fact 6                  # 720 (factorial)
fib 10                  # 89 (Fibonacci)
gcd 48 18               # 6 (greatest common divisor)
lcm 12 8                # 24 (least common multiple)
sqrt 25                 # 5 (integer square root)
isPrime 17              # true

# Number predicates
even 8                  # true
odd 7                   # true
ispositive 5            # true
```

### 4. List Operations

#### Basic List Functions

```lambda
# List construction and access
cons 1 [2, 3]          # [1, 2, 3]
head [1, 2, 3]         # 1
tail [1, 2, 3]         # [2, 3]
isnil []               # true
length [1, 2, 3, 4]    # 4

# List manipulation
append [1, 2] [3, 4]   # [1, 2, 3, 4]
reverse [1, 2, 3]      # [3, 2, 1]
take 3 [1, 2, 3, 4, 5] # [1, 2, 3]
drop 2 [1, 2, 3, 4, 5] # [3, 4, 5]
```

#### Higher-Order List Functions

```lambda
# Map, filter, fold
map (mult 2) [1, 2, 3, 4]           # [2, 4, 6, 8]
filter even [1, 2, 3, 4, 5, 6]      # [2, 4, 6]
foldl plus 0 [1, 2, 3, 4]           # 10
foldr mult 1 [2, 3, 4]              # 24

# List generation
range 5                             # [0, 1, 2, 3, 4]
enumFromTo 3 7                      # [3, 4, 5, 6, 7]
repeat 3 42                         # [42, 42, 42]
primes 20                           # [2, 3, 5, 7, 11, 13, 17, 19]
range 3 9                           # Built-in two-arg form (injected) → [3,4,5,6,7,8,9]
range2 2 4 12                       # Step = 2 (4-2) → [2,4,6,8,10,12]
range2 10 7 -2                      # Step = -3 → [10,7,4,1,-2]
```

#### List Utilities

```lambda
# Element access and searching
nth 2 [10, 20, 30, 40]             # 30
elem 3 [1, 2, 3, 4]                # true
find (gt 10) [5, 15, 3, 20]        # 15
any even [1, 3, 6, 7]              # true
all (lt 10) [1, 5, 8, 9]           # true

# Aggregation
sum [1, 2, 3, 4]                   # 10
product [2, 3, 4]                  # 24
maximum [3, 7, 2, 9, 1]            # 9
minimum [3, 7, 2, 9, 1]            # 2
```

#### Advanced List Operations

```lambda
# Zipping and unzipping
zip [1, 2, 3] [4, 5, 6]            # [(1,4), (2,5), (3,6)]
zipWith plus [1, 2, 3] [4, 5, 6]   # [5, 7, 9]
unzip [(1,4), (2,5), (3,6)]        # ([1,2,3], [4,5,6])

# Set operations
union [1, 2, 3] [3, 4, 5]          # [1, 2, 3, 4, 5]
intersect [1, 2, 3] [2, 3, 4]      # [2, 3]
difference [1, 2, 3, 4] [2, 4]     # [1, 3]
nub [1, 2, 2, 3, 1, 4]             # [1, 2, 3, 4] (remove duplicates)
```

### 5. Data Structures

#### Pairs

```lambda
# Pair construction and access
p = pair 10 20
first p                            # 10
second p                           # 20
swap p                             # pair 20 10
```

#### Maybe Type (Optional)

```lambda
# Maybe represents optional values
just 42                           # Some value
nothing                           # No value

# Safe operations
safehead [1, 2, 3]                # just 1
safehead []                       # nothing
safediv 10 2                      # just 5
safediv 10 0                      # nothing
safeDiv 10 2                      # alias (preferred casing)

# List tail safe variants
safeInit [1,2,3]                  # [1,2]
safeInitMaybe [1,2,3]             # just [1,2]
safeInitMaybe [1]                 # nothing

# Maybe operations
fromMaybe 0 (just 42)             # 42
fromMaybe 0 nothing               # 0
maybe 0 (mult 2) (just 21)        # 42
```

#### Either Type (Error Handling)

```lambda
# Either represents success/failure
right 42                          # Success value
left "error"                      # Error value

# Either operations
either (λe.0) (λv.v) (right 42)   # 42
either (λe.0) (λv.v) (left "err") # 0
```

## Infix Operators

Define custom infix operators with precedence and associativity:

```lambda
# Define operators
:infix + 6 left                    # Addition
:infix * 7 left                    # Multiplication (higher precedence)
:infix ^ 8 right                   # Exponentiation (right associative)

# Use infix notation (desugared to function application)
3 + 4 * 5                          # → plus 3 (mult 4 5) = 23
2 ^ 3 ^ 2                          # → exp 2 (exp 3 2) = 512 (right associative)

# Custom operators
:infix <> 5 left
<> = λx y.not (eq x y)
3 <> 4                             # → <> 3 4 → (λx y.not (eq x y)) 3 4 → true
```

### Built-in Special Operators

The interpreter includes two powerful built-in operators that provide essential functional programming patterns:

#### Pipeline Operator (`|>`)

The pipeline operator enables left-to-right function composition and data transformation:

```lambda
# Pipeline operator: a |> f |> g desugars to g (f a)
5 |> succ |> mult 2                # → mult 2 (succ 5) = 12

# Compare with nested function calls
mult 2 (succ 5)                    # Same result, but less readable

# Chaining list operations
[1, 2, 3, 4, 5] |> map (mult 2) |> filter (λx.gt x 5)
# → filter (λx.gt x 5) (map (mult 2) [1, 2, 3, 4, 5])
# → [6, 8, 10]

# Data processing pipelines
42 |> pred |> pred |> mult 3       # → mult 3 (pred (pred 42)) = 120
```

#### Function Composition Operator (`.`)

The composition operator enables right-to-left function composition:

```lambda
# Composition operator: f . g desugars to f (g x) when applied to x
double = mult 2
increment = succ
doubleInc = double . increment      # → λx.double (increment x)

doubleInc 5                         # → double (increment 5) = 12

# Multiple composition (right-associative)
f . g . h                           # → f (g (h x)) when applied to x

# Creating complex transformations
processNumber = mult 3 . succ . mult 2
processNumber 4                     # → mult 3 (succ (mult 2 4)) = 27

# Function composition in higher-order functions
map (mult 2 . succ) [1, 2, 3]       # → [4, 6, 8]
```

#### Pipeline vs Composition

```lambda
# Pipeline: left-to-right data flow (good for data processing)
data |> transform1 |> transform2 |> transform3

# Composition: right-to-left function building (good for creating reusable functions)
complexFunction = transform3 . transform2 . transform1

# Equivalent results:
5 |> succ |> mult 2                # Pipeline
(mult 2 . succ) 5                  # Composition

# Pipeline emphasizes the data flow
# Composition emphasizes function building
```

## Macro System

Define reusable patterns with the macro system:

```lambda
# Basic macros (pattern matching and substitution)
:macro (when $cond $body) => (if $cond $body I)
:macro (unless $cond $body) => (if $cond I $body)
:macro (square $x) => (mult $x $x)

# Using macros (expanded at parse time)
when (gt 5 3) (plus 1 2)           # → if (gt 5 3) (plus 1 2) I → 3
unless (iszero 0) 42               # → if (iszero 0) I 42 → I (identity)
square 7                           # → mult 7 7 → 49

# Advanced macros
:macro (for $var at $list do $body) => (map (λ$var.$body) $list)
for x at [1, 2, 3] do (mult x x)   # → map (λx.mult x x) [1, 2, 3] → [1, 4, 9]
```

### Macro System Enhancements (Recent)

The macro system has been extended beyond simple one-clause textual substitution to support more expressive, pattern-driven transformations:

1. Multi‑Clause (Alternation) Macros
   - Multiple definitions may share the same name.
   - The engine picks the first matching clause using precedence rules (see Ordering below).
        - Example:

    ```lambda
    :macro (max2 $a $b) when (geq $a $b) => $a
    :macro (max2 $a $b) => $b
    max2 5 3   # → 5 (guarded clause fires)
    max2 2 7   # → 7 (fallback clause)
    ```

2. Variadic / Rest Patterns
   - Use an ellipsis after a variable at the end of a pattern to capture zero or more trailing arguments.
   - Syntax: `$xs ...` (three-dot ellipsis recognized lexically).
   - Captured arguments are provided to the expansion as a list (Church-encoded list of arguments).
        - Example:

    ```lambda
    :macro (list $xs ...) => $xs
    list 1 2 3 4   # expands to the list [1,2,3,4]
    ```

3. Guards
   - Optional `when (<expr>)` between the pattern and the `=>` allows conditional selection among clauses.
   - Guard expressions can reference pattern variables (e.g. `$a`, `$b`).
   - A clause only matches if the pattern matches AND the guard does not evaluate to boolean false.
   - Example shown in max2 above.

4. Clause Ordering & Precedence
   - Clauses are ordered primarily by arity (more specific = more fixed arguments before a rest parameter).
   - Ties are broken by recency (most recently defined matching clause wins) to allow incremental refinement.

5. Pattern Variable Placeholders
   - During parsing, occurrences of `$name` in transformation (and guard) bodies are internally converted to placeholders, ensuring capture-safe substitution.
   - Integer literals inside macro bodies are delayed (kept as integers) until normal expression building, supporting lightweight numeric macros.

6. Rest Pattern Constraints
   - At most one rest variable per pattern and it must appear at the end.
   - Rest cannot appear in nested list subpatterns (future enhancement may relax this).

7. Display / Introspection
    - `:env macros` lists every clause including its guard (if any) and a `...` marker for rest variables.

#### Example Putting It Together

```lambda
:macro (assert-max $a $b) when (geq $a $b) => (assert (geq $a $b) $a)
:macro (assert-max $a $b) => (assert (geq $b $a) $b)
:macro (pipeline $first $rest ...) => ($rest ... $first)
```

### Planned / Future Improvements

These enhancements are on the roadmap to further strengthen the macro system:

- Hygiene & gensym (automatic alpha-renaming to prevent variable capture).
- Quasiquote / unquote / splicing for more ergonomic macro bodies.
- Richer guard evaluation semantics (full interpretation with short-circuiting, pattern predicates).
- Macro removal / redefinition controls (`:unmacro` or named groups).
- Namespacing and selective macro importing.
- Nested rest patterns and rest in sublists.
- Compile-time evaluation blocks.

### Performance Metrics Reference

The `:stats` command now exposes fine‑grained runtime counters and timers to help you understand performance characteristics of reductions:

| Metric | Meaning |
|--------|---------|
| TimeInCacheLookup | Elapsed ticks spent checking the evaluation cache |
| TimeInSubstitution | Time spent performing variable substitution / beta-reduction prep |
| TimeInEvaluation | Core CEK evaluation loop time (excluding forcing & cache lookups) |
| TimeInForcing | Time spent forcing (evaluating) thunks in lazy mode |
| NormalizeCEKCount | Number of normalization (evaluation) passes completed |
| CacheHits | Number of successful cache lookups (expression result reused) |
| CacheMisses | Number of times an expression wasn’t cached and had to be evaluated |
| TotalIterations | Cumulative machine steps across all evaluations in the session |
| Iterations | Machine steps for the last evaluated top‑level expression |
| SubstitutionExprCount | Count of expressions traversed during substitution operations |
| ThunkForceCount | How many thunks were forced (realized) |
| VarCounter | Internal counter for generating fresh variable names (used for avoiding clashes) |
| MaxRecursionDepth | Safety cut‑off to prevent runaway / infinite recursive expansions |

Notes:

- Timings are raw tick counts (convert with `TimeSpan.FromTicks` if needed) and are indicative rather than absolute wall time.
- `CacheHits` vs `CacheMisses` helps gauge effectiveness of memoization; a very low hit rate may signal highly unique expressions.
- A rapidly growing `SubstitutionExprCount` or `Iterations` value can indicate a need to refactor or introduce more sharing.
- If you routinely hit the `MaxRecursionDepth`, consider refactoring with tail recursion or increasing the limit in code (config command forthcoming).

## REPL Command Reference

This table is synchronized with the interpreter's internal command metadata (shown via `:help`).

| Command | Syntax | Description |
|---------|--------|-------------|
| :clear | `:clear [macros\|defs\|ops\|cache\|all]` | Clear state (default all). macros, defs, ops (infix), cache (memoization), or all (env+macros+ops+stats+caches) |
| :depth | `:depth [n]` | Show or set maximum recursion depth (range 10-10000) |
| :env | `:env [defs\|macros\|infix\|native\|all]` | Display environment (optionally filtered); default all |
| :exit | `:exit \| :quit` | Exit the interpreter |
| :help | `:help` | Show help summary |
| :infix | `:infix [op prec assoc]` | Define or list infix operators (assoc = left\|right) |
| :lazy | `:lazy on\|off` | Toggle lazy (on) vs eager (off) evaluation |
| :load | `:load <file>` | Load a .lambda file (may contain defs, macros, infix) |
| :log | `:log <file\|off\|clear>` | Log output to file, disable or clear current file |
| :macro | `:macro (<pattern>) => <body>` | Define a macro clause (supports guards & rest) |
| (cache) | `:clear cache` | Clear all memoization caches |
| :native | `:native on\|off\|show` | Toggle native arithmetic or list native primitives |
| :pretty | `:pretty on\|off` / `:pp on\|off` | Toggle pretty printing (numerals, lists, booleans) |
| :save | `:save <file>` | Persist current environment to a file |
| :stats | `:stats` | Show performance statistics & cache metrics |
| :step | `:step on\|off` | Toggle step-by-step CEK trace output |
| :test | `:test clear` / `:test result` | Reset or display structural equality test counters |

Tip: After heavy experimentation, run `:clear all` then `:load stdlib.lambda` to restore the baseline library. For only redefining functions without losing macro/infix definitions use `:clear defs`.

## Examples (Extended)

### Fibonacci Sequence

```lambda
# Recursive Fibonacci
fibRec = Y (λf n.if (iszero n) 1 (plus (f (pred n)) (f (pred (pred n)))))

# Iterative Fibonacci (more efficient)
fib 10                             # 89

# Fibonacci sequence
map fib (range 10)                   # [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
mapY (n -> [n, (fibY n)]) (range 10) # [[0, 1], [1, 2], [2, 3], [3, 5], [4, 8], [5, 13], [6, 21], [7, 34], [8, 55], [9, 89]]
```

### Prime Numbers

```lambda
# Check if number is prime
isPrime 17                         # true
isPrime 15                         # false

# Generate primes up to n
primes 30                          # [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

# Prime factorization (simplified)
factors = λn.filter (λd.eq (mod n d) 0) (enumFromTo 2 n)
factors 12                         # [2, 3, 4, 6, 12]
```

### List Processing

```lambda
# Process a list of numbers
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Get even squares
evenSquares = map square (filter even numbers)
# Result: [4, 16, 36, 64, 100]

# Sum of squares
sumOfSquares = sum (map square numbers)
# Result: 385

# Complex processing pipeline
result = sum (map square (filter odd (take 5 numbers)))
# Result: 35 (1² + 3² + 5² = 1 + 9 + 25)
```

### Church Encoding Examples

```lambda
# Church booleans in action
selectValue = λcond.if cond "success" "failure"
selectValue true                   # "success"
selectValue false                  # "failure"

# Church numerals as iterators
applyNTimes = λn f x.n f x
applyNTimes 3 succ 5              # 8 (apply successor 3 times to 5)
applyNTimes 4 (mult 2) 1          # 16 (multiply by 2, 4 times)
```

### Higher-Order Functions

```lambda
# Function composition chain
pipeline = compose (mult 3) (compose (plus 2) square)
pipeline 4                         # 54 ((4²+2)*3 = (16+2)*3 = 54)

# Partial application
add5 = partial plus 5
map add5 [1, 2, 3, 4]              # [6, 7, 8, 9]

# Function combinators
twice = λf x.f (f x)
twice succ 3                       # 5
twice (mult 2) 3                   # 12
```

## Advanced Usage

This section demonstrates sophisticated applications combining multiple features of the interpreter for real-world functional programming patterns.

### Complex Function Composition and Pipelines

```lambda
# Building sophisticated data processing pipelines
processNumbers = 
    filter (_ > 5) . 
    map (square . succ) . 
    takeWhile (_ < 100)

processNumbers [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# → [36, 49, 64, 81, 100] (filter > 5, then (x+1)², while < 100)

# Alternative with pipeline operator for readability
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 
    |> takeWhile (_ < 100)
    |> map (square . succ)
    |> filter (_ > 5)

# Combining macros with pipelines
:macro (between $x $low $high) => (and (geq $x $low) (leq $x $high))
:macro (clamp $min $max $x) => (if (lt $x $min) $min (if (gt $x $max) $max $x))

processData = 
    map (clamp 0 100) .
    filter (between _ 10 90) .
    map (_ |> mult 2 |> succ)

processData [-5, 15, 25, 105, 75]  # → [31, 51, 151] after clamping, filtering, transforming
```

### Advanced Macro Patterns

```lambda
# Creating domain-specific languages with macros
:macro (from $collection) => $collection
:macro (where $predicate) => (filter $predicate)
:macro (select $transform) => (map $transform)
:macro (orderBy $keyFunc) => (sortBy $keyFunc)
:macro (groupBy $keyFunc) => (groupWith (eq . $keyFunc))

# Usage: SQL-like queries in lambda calculus
people = [
    (record "name" "Alice" "age" 30 "dept" "Engineering"),
    (record "name" "Bob" "age" 25 "dept" "Sales"),
    (record "name" "Carol" "age" 35 "dept" "Engineering")
]

query = from people
    |> where (λp. gt (get "age" p) 25)
    |> select (λp. get "name" p)
    |> orderBy id

query  # → ["Alice", "Carol"]

# Building a simple arithmetic DSL
:macro (num $n) => $n
:macro (add $a $b) => (plus $a $b)
:macro (mul $a $b) => (mult $a $b)
:macro (var $name) => (λenv. lookup $name env)

# Expression evaluator
eval = λexpr env. expr env

# Usage
formula = add (mul (var "x") (num 2)) (var "y")
environment = fromList [("x", 5), ("y", 3)]
eval formula environment  # → 13 (5*2 + 3)
```

### Integration Patterns

```lambda
# Combining all features for a complete application
:macro (app $config $routes) => (λrequest. dispatch $routes request)
:macro (route $pattern $handler) => (pair $pattern $handler)

# Web server simulation using lambda calculus
webApp = app 
    (config "port" 8080 "host" "localhost")
    [
        route "/api/users" (λreq.
            req |> getPath |> parseUserId |> 
            maybe (error404) (λid. 
                users |> find (eq id . getId) |>
                maybe (error404) (toJson))),
        
        route "/api/health" (λ_. ok "Server is healthy"),
        
        route _ (λ_. error404)
    ]

# Request processing pipeline
processRequest = λreq.
    req |> validateHeaders |>
    bind auth |>
    bind (webApp) |>
    either errorResponse successResponse

# This demonstrates how lambda calculus can express
# complex application logic with proper error handling,
# data transformation, and modular design patterns
```

## Performance Features

### Lazy vs Eager Evaluation and Macros

Macros expand at parse time before evaluation mode (lazy/eager) matters. After expansion:

- Lazy mode (default) defers evaluation of arguments and macro-expanded bodies until needed.
- Switching with `:lazy off` makes evaluation eager; previously created thunks are forced as they appear.
- Guards in multi-clause macros are evaluated under the current mode (a false value blocks the clause). Currently, guard evaluation short‑circuits only if the top-level evaluates to Church false; deeper reductions still follow normal evaluation semantics.

Guidelines:

- Prefer lazy mode for large or infinite ranges (e.g. `[1 .. 1000000]` with `take 10`).
- Disable laziness (`:lazy off`) when benchmarking deterministic CPU-heavy pure functions to reduce overhead of thunk bookkeeping.
- If a macro expansion intentionally builds large intermediate expressions that are immediately consumed, eager mode can expose performance differences for optimization.

### Native Arithmetic

When enabled (`:native on`), the interpreter uses optimized native operations for Church numerals:

```lambda
# These operations are accelerated when native arithmetic is enabled:
plus, minus, mult, div, mod        # Basic arithmetic
succ, pred, iszero                 # Successor/predecessor
lt, leq, eq, geq, gt, neq          # Comparisons
max, min, sqrt, random             # Additional functions
alphaEq                            # Alpha-equivalence (normalize both; binder names ignored)
```

#### Alpha Equivalence (alphaEq)

`alphaEq` is a native helper that returns a Church boolean (`true` / `false`) indicating whether two expressions are *semantically equal up to normalization and alpha-equivalence*.

Current pipeline (implementation detail, but useful to know):

1. Force thunks on both sides (only as far as needed to expose head constructors).
2. Normalize both expressions (bounded beta-reduction with:
    - Inlining of top-level named lambda bindings for deeper combinator simplification (e.g. `K`, `S`, etc.).
    - Memoization & depth / cycle guards to avoid runaway expansion.)
3. Perform alpha-equivalence comparison (binder names are ignored; structure & binding topology must match).
4. If alpha-equivalent normalized forms fail, a fallback direct structural check on original (pre-normalized) forms may be used for diagnostics.

Characteristics:

- Ignores superficial binder name differences: `alphaEq (λx.x) (λy.y)` → `true`.
- Distinguishes genuinely different structure (no eta-reduction: `λx.f x` ≠ `f`).
- Reduces common combinator compositions so higher-order identities hold (e.g. `(S K K) v` equals `v`).
- Treats Church-encoded lists and their explicit `cons`/`nil` forms uniformly only after normalization; distinct encodings that do not normalize to the same shape still differ.
- Safe for recursive definitions via Y so long as expansion reaches a stable normalized comparison within the configured limits.

Instrumentation:

- Every `alphaEq` invocation increments counters (`StructEqCalls`, `StructEqSuccesses`).
- View with `:test result`, reset with `:test clear`.
- Useful for tracking test coverage density in large spec suites.

Practical examples:

```lambda
alphaEq (pair 1 (cons 2 nil)) (pair 1 (cons 2 nil))          # true
alphaEq [1,2,3] (cons 1 (cons 2 (cons 3 nil)))               # true
alphaEq (λx.plus x 1) (λy.plus y 1)                          # true (alpha-insensitive)
alphaEq (λx.plus x 1) (λy.plus y (succ 0))                   # false (different body after norm)
alphaEq ((S K K) 5) 5                                        # true (combinator reduces)
alphaEq (λx.f x) f                                           # false (no eta)
```

Notes & Limits:

- Not an extensional (eta-complete) equality: add your own eta rule if needed.
- Normalization is bounded; extremely large self-expanding macros may yield a conservative `false` if forms cannot be fully reduced within limits.
- Designed for test/regression usage rather than user-facing proof of equivalence.

If you require different equivalence semantics (e.g. eta-equivalence or observational equivalence), layer a custom checker on top of normalized forms.


### Caching and Memoization

The interpreter includes multiple caching layers:

- **Substitution cache**: Speeds up variable substitution
- **Evaluation cache**: Memoizes expression evaluation
- **Free variable cache**: Caches free variable analysis
- **Expression interning**: Reduces memory usage

### Statistics and Monitoring

Use `:stats` to view detailed performance information:

```shell
:stats
# Shows cache hit rates, evaluation counts, memory usage, etc.
```

## Step Tracing & Debugging

Enable detailed CEK reductions with:

```shell
:step on
```

You will see `Step` lines (green/yellow in CLI, class `log-step` in Web UI) for each continuation application or reduction. Combine with `:pretty off` for raw lambda forms.

| Technique | Command(s) | Purpose |
|-----------|-----------|---------|
| Enable tracing | `:step on` | Show each evaluation step |
| Disable tracing | `:step off` | Return to concise mode |
| Inspect environment | `:env` | Verify bindings contributing to steps |
| Abort multi-line input | `:cancel` | Prevent accidental huge traces |

Common pattern to isolate a problematic expansion:

```shell
:clear all; :load stdlib.lambda; :step on; :pretty off
mySuspiciousExpr
```

Turn tracing off immediately once you've captured enough lines to avoid performance overhead.

## Performance Cookbook

| Scenario | Recommended Setup | Reason |
|----------|-------------------|--------|
| Pure lambda benchmarking | `:lazy off; :native off; :pretty off` | Removes thunk + native shortcut overhead |
| Demonstrate laziness | `:lazy on; take 10 [0 .. 1000000]` | Shows finite prefix of huge range |
| Maximizing cache hits | Evaluate same function repeatedly then `:stats` | Observe CacheHits growth |
| Macro expansion audit | `:pretty off; :step on` once | Inspect raw expanded forms |
| Structural regression tests | Use `alphaEq` in macros + `:test result` | Track success count trend |

Quick timing heuristic: compare `Iterations` between implementations of the same function (e.g., naive vs tail-recursive) under identical settings.

## Embedding & Programmatic API

Minimal embedding example:

```csharp
var logger = new LambdaCalculus.Logger { EnableBuffering = true };
var interp = new LambdaCalculus.Interpreter(logger: logger);
await interp.LoadFileIfExistsAsync("stdlib.lambda");
var (ast, value) = await interp.ProcessInputAsync("succ 41");
Console.WriteLine(interp.Format(value)); // 42
```

Registering a native primitive:

```csharp
interp.RegisterNativeFunction("triple", (op, args, env) =>
{
    if (args.Count == 1 && interp.TryGetChurchInt(args[0], env, out var n))
        return interp.MakeChurchNumeral(n * 3);
    return null; // Not handled
});
```

Chained processing (commands + expression):

```csharp
await interp.ProcessInputAsync(":lazy off; :native on; plus 20 22");
```

Buffered logs:

```csharp
foreach (var line in logger.GetBufferSnapshot()) Console.WriteLine(line);
```

## Multi-user & Deployment Strategies

Current Web UI: single shared interpreter instance (stateful). Options to scale:

| Strategy | Isolation | Outline | Pros | Cons |
|----------|-----------|---------|------|------|
| Per-request new instance | Full | Instantiate `Interpreter` per eval | No state bleed | High GC & start cost |
| Session-scoped | Medium | Dictionary keyed by session ID; expire LRU | Persistent user state | Cleanup complexity |
| Pooled | Medium | Pool of warm interpreters; `:clear` before checkout | Amortized init | Risk of incomplete reset |
| Stateless eval service | Logical | Reject definitions; evaluate single expr with ephemeral env | Horizontal scaling | Loses interactive definitions |

Hardening checklist:

- Enforce max iterations / time (wrap `ProcessInputAsync` with cancellation).
- Limit macro clause count + expansion depth.
- Restrict file load paths (whitelist) for `:load` / `/api/load`.
- Add auth & rate limiting at reverse proxy.

Observability: subscribe to `Logger.Subscribe(line => Forward(line));` to pipe logs to structured telemetry (ensure async, non-blocking).


## Building and Running

### Prerequisites (Build & Run)

- .NET 8.0 or later
- C# compiler

### Building

```bash
# Build the project
dotnet build

# Run in development mode
dotnet run

# Build release version
dotnet build -c Release
```

### Running

```bash
# Start interactive mode
./lambda-cek

# Load specific files
./lambda-cek stdlib.lambda tests.lambda

# Run with native arithmetic enabled by default
./lambda-cek
:native on
```

The interpreter automatically loads `stdlib.lambda` if it exists in the current directory, providing access to the full standard library.

## File Format

Lambda files (`.lambda` extension) support:

- Lambda expressions and definitions
- Comments (lines starting with `#` or text after `#`)
- Multi-line expressions
- Macro definitions
- Infix operator definitions

Example file structure:

```lambda
# My lambda file
# Define some utilities
id = λx.x
compose = λf g x.f (g x)

# Define infix operators
```lambda
:infix ∘ 9 right
∘ = compose

# Use the definitions
twice = f -> f ∘ f
result = twice succ 5  # 7
```

This interpreter provides a complete environment for exploring lambda calculus, functional programming concepts, and advanced language features while maintaining high performance through lazy evaluation and intelligent caching.

## Pretty Printing

The interpreter includes an optional pretty-print layer (enabled by default) that rewrites common Church encodings into familiar surface forms for readability.

### What It Formats

1. Church numerals  
    Structure: `λf.λx.f^n x` rendered as the decimal integer `n`. Example: `λf.λx.f (f (f x))` → `3`.
2. Church booleans  
    `λa.λb.a` → `true`, `λa.λb.b` → `false` (variable names may differ; the structure determines the value).  
3. cons / nil lists  
    Nested applications of `cons` ending in `nil` render as `[a, b, c]`.  
    Example: `cons 1 (cons 2 (cons 3 nil))` → `[1, 2, 3]`.
4. Church-encoded lists  
    `λf.λz.f a1 (f a2 (... (f an z)))` also renders as `[a1, a2, ..., an]`.
5. Thunks  
    Unforced: `<thunk: ...>`  
    Forced: `<forced: renderedValue>`
6. Cycles / repeated references  
    Already-visited nodes during printing show as `<cycle>` to avoid infinite output.

### Booleans vs Conditionals

Only the canonical Church boolean forms print as `true` / `false`. Any other two-argument selector (e.g. `λx.λy.y x`) keeps its raw lambda form.

### Truncation

Very large output is truncated at 5000 characters with a `... (output truncated)` suffix to protect the REPL.

### Toggling

Use the command:

```shell
:pretty off   # Show raw lambda structures
:pretty on    # Re‑enable formatting
```

Disabling pretty printing is useful for debugging structural differences or confirming alpha‐conversion results.

### Pretty Printing Examples

| Raw Form                                   | Pretty Printed |
|--------------------------------------------|----------------|
| `λf.λx.x`                                  | `0`            |
| `λf.λx.f (f x)`                            | `2`            |
| `λa.λb.a`                                  | `true`         |
| `λp.λq.q`                                  | `false`        |
| `cons 1 (cons 2 nil)`                      | `[1, 2]`       |
| `λf.λz.f 4 (f 5 (f 6 z))`                  | `[4, 5, 6]`    |
| `<thunk: λf.λx.f x>` (unforced)            | `<thunk: 1>`   |
| `<forced: λf.λx.f (f x)>` (already forced) | `<forced: 2>`  |

### Limitations / Notes

- Church numeral detection currently expects the exact two‑abstraction shape; if you manually construct numerals with additional wrapping lambdas they will not collapse to integers.  
- Boolean detection is name-agnostic: `λt.λf.f` still prints `false`.  
- If you redefine `cons` or `nil` to non-standard meanings, list rendering may become misleading.  
- Extremely deep or cyclic structures may show `<cycle>` early to prevent runaway traversal.  
- Pretty printing is orthogonal to evaluation; disabling it does not change semantics or performance of normalization/evaluation (aside from minor formatting cost).

### When to Turn It Off

Disable with `:pretty off` when:

- Verifying the *exact* lambda structure (e.g., in macro expansion debugging).  
- Measuring performance without the formatting overhead (minor but measurable for very large outputs).  
- Teaching / demonstrations where raw encodings are pedagogically important.

Re‑enable with `:pretty on` once finished.

---

## Project Overview

This project is a high-performance, feature-rich lambda calculus interpreter implemented in C#. It is designed for students, researchers, and enthusiasts interested in functional programming, language theory, and interpreter implementation. The interpreter is based on the CEK (Control, Environment, Kontinuation) machine model, supporting both lazy and eager evaluation, a macro system, infix operators, and a comprehensive standard library.

**Goals:**

- Provide a practical and educational tool for exploring lambda calculus and functional programming concepts.
- Offer a modern, extensible playground for experimenting with language features, macros, and evaluation strategies.
- Achieve high performance through native optimizations and intelligent caching.

## How It Works

**Evaluation Engine:**

- The interpreter uses a CEK machine, which models computation as a stack of states (control, environment, continuation). This enables efficient evaluation, supports both strict and lazy semantics, and makes it easy to implement advanced features like thunks and continuations.

**Lazy vs. Eager Evaluation:**

- By default, the interpreter uses lazy evaluation (thunks), only computing values when needed. You can switch to eager evaluation with `:lazy off`.

**Macros and Infix Operators:**

- Macros are expanded at parse time, allowing for powerful syntactic abstractions and domain-specific language features. Infix operators are user-definable with custom precedence and associativity, parsed using a Pratt parser.

**Standard Library:**

- The standard library (`stdlib.lambda`) is loaded automatically and provides a rich set of combinators, arithmetic, list operations, and more.

For more theoretical background, see `THEORY.md`.

## Contributing

Contributions are welcome! To get started:

1. **Clone the repository and build:**

    ```bash
    git clone <repo-url>
    cd lambda-cek-Y
    dotnet build
    ```

2. **Run tests:**
    - (Add instructions here if you have a test suite, e.g., `dotnet test`)

3. **Style guidelines:**
    - Use clear, descriptive names and add comments for new features.
    - Keep code modular; new primitives or macros should be added in their respective files.

4. **Adding features:**
    - To add new primitives, see `Interpreter.Eval.cs`.
    - For new macros or infix operators, update `stdlib.lambda` or use the REPL.

5. **Pull requests:**
    - Please describe your changes and reference any related issues.

## Troubleshooting & FAQ

**Q: I get an 'Evaluation exceeded maximum iterations' error.**
A: This usually means your code has an infinite loop or non-terminating recursion. Check for missing base cases or undefined variables.

**Q: Why do I get a parse error about semicolons or commas?**
A: Semicolons are only allowed at the top level. Commas must separate parameters or list elements, not appear at the end or in the wrong context.

**Q: How do I debug my code?**
A: Use `:step on` for step-by-step evaluation, `:stats` for performance info, and `:log <file>` to log output. Disable pretty printing with `:pretty off` to see raw structures.

**Q: How do I add new native functions?**
A: Extend the `TryNativeArithmetic` method in `Interpreter.Eval.cs`.

**Q: How do I reset the environment?**
A: Use `:clear` (or `:clear all`) to reset everything, or selectively `:clear defs` / `:clear macros` / `:clear ops`.

## Design Decisions

- **CEK Machine:** Chosen for its clarity and efficiency in modeling lambda calculus evaluation, supporting both strict and lazy semantics.
- **Church Encoding:** Used for numbers, booleans, and lists to stay true to pure lambda calculus, with pretty-printing for usability.
- **C# Implementation:** Leverages .NET performance, strong typing, and modern tooling.
- **Extensibility:** Macro and infix systems allow users to extend the language without modifying the core interpreter.
**Current Limitations (Updated):**

- No floating-point or rational literal support yet (only Church integers; extension planned).
- No hygienic macro system or quasiquotation (planned).
- Module/import system absent (single global environment unless manually isolated).
- Error objects are string-based (structured diagnostics planned).
- Sandboxing (timeouts, memory quotas) not yet enforced—use caution in multi-user setups.
- Pattern matching construct (`match`) not yet implemented (macros approximate use cases).

## License

This project is licensed under the MIT License. See `LICENSE` for details.

## Distribution & Packaging (New)

Artifacts planned / available:

- NuGet package: `LambdaCalculus.Interpreter` (core library, CEK evaluator, parser, macros). Install with:
    `dotnet add package LambdaCalculus.Interpreter`
- CLI tool project (`src-cli`) provides interactive REPL (build: `dotnet run --project src-cli`).
- Web host (`src-web` / `src-webui`) serves browser UI.
- WASM build (`src-wasm`) experimental Blazor WebAssembly host for in-browser evaluation (no server).

### Roadmap

- Add GitHub Actions workflow to pack & push NuGet on tagged release.
- Publish WASM demo via GitHub Pages (copy `wwwroot` + `_framework`).
- Provide minimal JS interop API (evaluate, normalize, stats) for embedding.

## Build & Run Guide

This section summarizes how to build and run each form of the interpreter: core library (NuGet), CLI, Web API, Web UI, and WASM.

### Prerequisites

- .NET 8 SDK (required). Optional: .NET 9 preview for multi-target build; ignore preview warning if not needed.
- PowerShell (examples assume Windows `pwsh`).
- (Optional) Docker Desktop for container build of Web UI.
- (Optional for WASM static hosting) A static file server (`dotnet-serve`, `npx serve`, or any HTTP server).

### 1. Core Library (NuGet Package)

Build only:

```powershell
dotnet build src/lambda-cek.csproj -c Release
```

Pack (produces `.nupkg` in `artifacts`):

```powershell
dotnet pack src/lambda-cek.csproj -c Release -o artifacts
```

Override version:

```powershell
dotnet pack src/lambda-cek.csproj -c Release -o artifacts /p:PackageVersion=0.1.1
```

Consume in another project:

```powershell
dotnet add package LambdaCalculus.Interpreter --version 0.1.0
```

### 2. CLI REPL (`src-cli`)

Run (Debug):

```powershell
dotnet run --project src-cli/lambda-cek.cli.csproj
```

Run (Release):

```powershell
dotnet run -c Release --project src-cli/lambda-cek.cli.csproj
```

Load extra lambda files at startup:

```powershell
dotnet run --project src-cli/lambda-cek.cli.csproj -- mydefs.lambda tests.lambda
```

Inside REPL: use `:help`, exit with `:quit` / `:exit`.

### 3. Web API (`src-web`)

Starts a minimal HTTP host (e.g., evaluation endpoints if implemented):

```powershell
dotnet run --project src-web/lambda-cek.web.csproj
```

If HTTPS dev cert not trusted:

```powershell
dotnet dev-certs https --trust
```

Override URL/port:

```powershell
dotnet run --project src-web/lambda-cek.web.csproj -- --urls http://localhost:5055
```

### 4. Web UI (`src-webui`)

Interactive browser-based REPL with multi-tab output, search, filters.

Run:

```powershell
dotnet run --project src-webui/lambda-cek.webui.csproj
```

Navigate to the printed local URL (commonly <http://localhost:5000>).

#### Docker Build (Web UI)

Using the build script (includes Docker unless `-NoDocker`):

```powershell
./build.ps1
```

Manual build & run:

```powershell
docker build -t lambda-cek-webui -f src-webui/Dockerfile .
docker run -p 8080:8080 --name lambda-cek-webui lambda-cek-webui
```

Open <http://localhost:8080>.

### 5. WASM (Blazor WebAssembly, `src-wasm`)

Publish:

```powershell
dotnet publish -c Release src-wasm/lambda-cek.wasm.csproj -o artifacts/wasm
```

Serve the published folder:

```powershell
dotnet tool install --global dotnet-serve   # once
dotnet serve -d artifacts/wasm
```

Visit the served URL; open browser console to confirm initialization.

### 6. Unified Build Script (`build.ps1`)

Examples:

```powershell
# Build everything + pack NuGet + publish WASM + build Docker image
./build.ps1 -Pack -Wasm

# Skip Docker
./build.ps1 -Pack -Wasm -NoDocker

# Validate package content & abort if missing files
./build.ps1 -Pack -Validate

# Continue pack even if validation fails
./build.ps1 -Pack -Validate -SkipPackOnError
```

Outputs:

- NuGet package → `artifacts/*.nupkg`
- WASM publish → `artifacts/wasm`
- Docker image → `lambda-cek-webui` (run with `docker run -p 8080:8080 lambda-cek-webui`)

### 7. Running `tests.lambda`

CLI:

```powershell
dotnet run --project src-cli/lambda-cek.cli.csproj -- tests.lambda
```

In REPL: `:load tests.lambda`

### 8. Troubleshooting

| Issue | Cause | Fix |
|-------|-------|-----|
| NU5019 README not found | Wrong relative path in `.csproj` | Confirm `src/lambda-cek.csproj` uses `..\README.md` |
| NETSDK1057 preview warning | net9.0 preview | Ignore or remove `net9.0` from `TargetFrameworks` |
| HTTPS trust prompt | Dev cert untrusted | `dotnet dev-certs https --trust` |
| Port in use | Conflict with existing process | Use `--urls` to pick another port |
| WASM blank page | Not served over HTTP | Use a static server (not `file://`) |

### 9. Quick Command Reference

| Target | Command |
|--------|---------|
| Build all | `dotnet build -c Release` |
| Pack | `dotnet pack src/lambda-cek.csproj -c Release -o artifacts` |
| CLI REPL | `dotnet run --project src-cli/lambda-cek.cli.csproj` |
| Web UI | `dotnet run --project src-webui/lambda-cek.webui.csproj` |
| Web API | `dotnet run --project src-web/lambda-cek.web.csproj` |
| WASM publish | `dotnet publish -c Release src-wasm/lambda-cek.wasm.csproj -o artifacts/wasm` |
| Docker image | `docker build -t lambda-cek-webui -f src-webui/Dockerfile .` |

---

If you need a GitHub Actions CI workflow or JS interop examples for WASM, open an issue or continue the conversation.

