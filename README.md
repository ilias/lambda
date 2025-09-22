# Lambda Calculus Interpreter

High-performance lambda calculus environment in C# (.NET 8/9): lazy/eager CEK machine, Pratt parser (user-defined infix operators), macro system (multi-clause, structural, guards, variadic), structural equality helpers, optional native fast paths.

## Features

- Lazy & eager evaluation (CEK)
- User-defined infix operators (Pratt)
- Macros: structural patterns, guards, variadic/rest
- Structural helpers: `alphaEq`, `betaEq`, `hashEq`, `etaEq`
- Native numeric & list fast paths (`:native on`)
- ~200 stdlib definitions (`stdlib.lambda`)
- CLI, Web API, streaming Web UI (SSE / WebSocket)
- NuGet package + Docker (Web UI)
- Debug print helper with optional label: `print expr` or `print "label" expr` → logs `Print [label ]value`

## Docs

| Topic | File |
|-------|------|
| Language & macros | docs/LANGUAGE.md |
| REPL / commands & metrics | docs/COMMANDS.md |
| Build / embedding / Docker / streaming | docs/COMPILER.md |
| Theory & background | docs/THEORY.md |
| Standard library | stdlib.lambda |
| Full monolith (archive) | FULL_DOC.md |

## Quick Start

```powershell
dotnet build
dotnet run --project src-cli --
succ 41  # -> 42
```

Web UI:

```powershell
dotnet run --project src-webui --
```

Open <http://localhost:5000>

API:

```powershell
dotnet run --project src-web -- --urls http://localhost:5055
curl "http://localhost:5055/eval?expr=succ%2041"
```

## Embed

```csharp
var it = new LambdaCalculus.Interpreter(logger: new LambdaCalculus.Logger());
await it.LoadFileIfExistsAsync("stdlib.lambda");
var (_, r) = await it.ProcessInputAsync("succ 41");
Console.WriteLine(r); // 42
```

## Layout

| Project | Path | Purpose |
|---------|------|---------|
| Core | src/ | Engine (parser, CEK, macros, natives) |
| CLI | src-cli/ | REPL / file runner |
| Web API | src-web/ | HTTP eval/load endpoints |
| Web UI | src-webui/ | Browser REPL + streaming logs |

## Testing

Add cases to `tests.lambda` (use `alphaEq` / `betaEq`).

## Contributing

PRs welcome; document new features in `docs/`.

## License

MIT

---
Full reference lives in `docs/` and `FULL_DOC.md`.
<!-- Duplicate heading/feature block removed (content already summarized above). Keeping a single canonical intro. -->

This table is synchronized with the interpreter's internal command metadata (shown via `:help`).

| Command | Syntax | Description |
|---------|--------|-------------|
| :clear | `:clear [macros\|defs\|ops\|cache\|all]` | Clear state (default all). macros, defs, ops (infix), cache (memoization), or all (env+macros+ops+stats+caches) |
| :depth | `:depth [n]` | Show or set maximum recursion depth (range 10-10000) |
| :env | `:env [defs\|macros\|infix\|native\|all]` | Display environment (optionally filtered); default all |
| :exit | `:exit \| :quit` | Exit the interpreter |
| :help | `:help` | Show help summary |
| :module | `:module <load\|list\|reload\|unload\|alias\|import\|with\|clear-imports> ...` | Module management: load files into a namespaced alias (supports hierarchical submodules), list, reload/unload, alias rename (registry only), selective import into unqualified scope, temporary with-scope eval, clear imported names. Inside module files, support `:module export {..}` and `:module hide {..}` to control published symbols. |
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
| :doc | `:doc <name>` / `:doc <name> = "text"` / `:doc export <file>` | Symbol docs: show, set, and export collected docs (supports inline `## name: text` in files) |
| :find | `:find <name>` | Locate a symbol across top-level, modules, macros, natives, infix |
| :grep | `:grep <pattern>` | Case-insensitive substring search across defs, modules, macros, natives, infix |
| :hist | `:hist [n]` | Show last n (default 20) entered top-level inputs |
| :repeat | `:repeat <index\|-k>` | Re-run history entry by index or negative offset -1 means last |
| :reload | `:reload` | Reload the most recently :load'ed file |
| :last | `:last` | Show the last evaluated expression again |

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

### Debug / IO Helpers & Pretty/Show Utilities

There are two layers of lightweight debugging helpers that preserve the original value so they compose inside pipelines without disturbing evaluation order.

1. Core primitive: `print`
2. Stdlib wrappers & macros: `trace`, `traceVal`, `traceSilent`, `tap`, `tapv`, plus lazy-safe variants `traceDelay`, `traceDelayVal`, `tapd`, `tapdv`.

`print` eagerly evaluates its argument (or labeled second argument), pretty prints it (respecting `:pretty`), logs a colored line beginning with `Print`, and returns the value unchanged. Label form: `print "label" expr`.

Wrappers (lazy-safe instrumentation):

```lambda
trace        = λlabel.λx.print label x      # labeled value, returns x
traceVal     = λx.print x                   # value only, returns x
traceSilent  = λlabel.λx.x                  # no-op (keep shape for conditional insertion)
traceDelay   = λlabel.λth.trace label (force th)      # delayed computation with label
traceDelayVal= λth.traceVal (force th)                # delayed computation value-only
```

Pipeline-oriented macros (expand at parse time):

```lambda
expr |> tap   "lbl"  ==> trace "lbl" expr        # labeled
expr |> tapv         ==> traceVal expr            # unlabeled
expr |> tapd  "lbl"  ==> traceDelay "lbl" (delay expr)   # labeled, lazy-safe
expr |> tapdv        ==> traceDelayVal (delay expr)       # unlabeled, lazy-safe
```

Examples:

```lambda
print 5                  # Print 5
print "dude" 5          # Print dude 5
(factorial 5) |> print |> succ          # Print 120 -> 121
map succ [1,2,3] |> print "vec"        # Print vec [2,3,4]

5 |> tap "seed" |> succ |> tapv        # labeled then value-only
let x = 3 in x |> tap "x" |> square    # observe x before squaring
(trace "mid" (succ 4)) |> succ          # 6 (with mid label on 5)
```

Choosing a helper:

| Helper | Prints | Returns | Use Case |
|--------|--------|---------|----------|
| `print` | value (optional label) | value | Ad hoc REPL probing |
| `trace` | label + value | value | Inline debug w/ label context |
| `traceVal` | value | value | Quick numeric / structural probe |
| `traceSilent` | nothing | value | Toggle off traces without editing pipeline shape |
| `tap` (macro) | label + value | value | Pipeline style (eager) |
| `tapv` (macro) | value | value | Pipeline style unlabeled (eager) |
| `traceDelay` | label + value (forces thunk) | value | Delay expression until trace site (avoid premature forcing) |
| `traceDelayVal` | value (forces thunk) | value | Lazy-safe value-only variant |
| `tapd` (macro) | label + value | value | Pipeline macro building a delayed trace |
| `tapdv` (macro) | value | value | Pipeline delayed value-only |

Tip: Replace `tap` with `traceSilent` (via a macro edit) or swap `tap`→`tapd` to make instrumentation lazy-safe without changing shape. Use delayed forms inside both branches of a large recursive conditional to prevent unnecessary evaluation.

### Modules & Imports (REPL)

The REPL supports loading lambda files as modules under an alias, exposing qualified names `ALIAS::name` and offering selective imports into the unqualified scope.

Quick tour:

```lambda
:module load "math.lambda" as M
M::pi                       # qualified access
:module import M::{add as addM, double}
addM 1 2                    # imported with rename
double 21                   # imported without rename
:module with M => (add a 1) # temporary with-scope for a single expression
:module list                # see loaded modules
:module reload M            # reprocess source file and update module symbols
:module unload M            # remove qualified symbols; imports remain

Submodules:

- A module file can load its own submodules using `:module load` inside the file.
- When loading a parent module as `A`, any submodule loaded inside (e.g., `:module load "B.lambda" as B`) is registered as a hierarchical alias `A.B`, and its qualified names appear as `A::B::name`.
- Paths used inside a module file are resolved relative to that file’s directory.
- `:module import` and `:module with` accept dotted aliases: `:module import A.B::{x as xB}`.
- `:module unload A` also unloads `A.B`, `A.B.C`, etc. Imported unqualified names persist.

Example:

```lambda
# submodA.lambda
:module load "submodB.lambda" as B
a = 1
ax = plus a B::x     # resolves to A::a and A::B::x when loaded as A

# submodB.lambda
x = 99
inc = \n. plus n 1

# REPL
:module load "submodA.lambda" as A
A::a            # 1
A::B::x         # 99
A::ax           # 100
:module import A.B::{inc as incB}
incB 41         # 42
:module unload A   # removes A::* and A::B::*; incB persists
```

Notes:

- Qualified identifiers use the form `ALIAS::name`.
- `:module alias OLD as NEW` renames the registry key; previously published qualified names keep their original alias.
- Imports copy the current value into the unqualified scope (they persist after `:module unload`).
- `:module with A => expr` is a command (not an expression); use it at the top level to evaluate `expr` with all `A` names temporarily mapped unqualified.
- Name rewriting inside a loaded module is binding‑aware: only free references to names defined by that module are qualified; bound variables are not.

### Show / Pretty Helper Additions

To disambiguate Church encodings (`0` vs `false` vs `nil` / `nothing`) during debugging, the stdlib offers lightweight tagged renderers that produce ASCII lists (strings):

| Helper | Input | Output Shape | Notes |
|--------|-------|--------------|-------|
| `showBool b` | Bool | `[116,114,117,101]` or `[102,97,108,115,101]` | Prints "true" / "false" |
| `showMaybe showA m` | Maybe A | chars for "Nothing" or "Just " ++ showA x | Caller supplies element renderer |
| `showEither showL showR e` | Either L R | "Left " ++ showL l OR "Right " ++ showR r | Disambiguates side |

Head character ASCII codes (for simple structural tests): `true -> 116 (t)`, `false -> 102 (f)`, `Just -> 74 (J)`, `Nothing -> 78 (N)`, `Left -> 76 (L)`, `Right -> 82 (R)`.

### Maybe / Either Monadic & Applicative Helpers

New combinators standardize chaining patterns:

| Type | Return | Helpers |
|------|--------|---------|
| Maybe | `nothing` / `just x` | `maybeBind`, `maybeReturn` (= `just`), `maybeAp`, `maybeMap2`, alias `mbind` |
| Either | `left e` / `right x` | `eitherBind`, `eitherReturn` (= `right`), `eitherAp`, `eitherMap`, `eitherMapLeft`, `eitherMap2`, alias `ebind` |

Examples:

```lambda
maybeBind (just 3) (λx.just (plus x 1))        # just 4
eitherAp (right succ) (right 4)                # right 5
maybeMap2 plus (just 2) (just 3)               # just 5
eitherBind (left 1) (λx.right (plus x 1))      # left 1 (short-circuits)
```

### Deeper Usage Patterns

```lambda
# 1. Conditional instrumentation (avoid double recursion evaluation)
fib = Y (λf n.
        if (n <= 2)
                1
                ( (n - 1 |> tapd "fib-1" |> f)
                    + (n - 2 |> tapd "fib-2" |> f)))

fib 10  # Uses delayed traces so only taken branches force values

# 2. Maybe pipeline with applicative style (no manual pattern matching)
add3 = maybeMap2 (λa b.plus a b) (just 10) (just 5)   # just 15

# 3. Composing three independent Maybes (Applicative composition law example)
comp = λf g x.f (g x)
fs = just succ
gs = just (mult 2)
xs = just 5
lhs = maybeAp (maybeAp (maybeAp (just comp) fs) gs) xs   # just (succ (mult 2 5)) = just 11
rhs = maybeAp fs (maybeAp gs xs)
alphaEq lhs rhs  # true

# 4. Either error accumulation chain (short-circuits on first Left)
step1 = λx.right (plus x 2)
step2 = λx.if (eq (mod x 5) 0) (left "div-by-5") (right (mult x 3))
eitherBind (eitherBind (right 7) step1) step2    # right 27
eitherBind (eitherBind (right 8) step1) step2    # left "div-by-5"

# 5. show* helpers inside a trace label (runtime-safe tagging)
value = just 42
trace "maybe" (showMaybe (λn.[110,61] ++ [n]) value)  # Prints tagged representation; returns string

# 6. Lazy-safe instrumentation of an expensive branch only when chosen
expensive = λn.(range (mult n 1000)) |> length  # pretend heavy list build
decide = λflag.if flag (traceDelay "heavy" (delay (expensive 5))) 0
decide true   # Forces and prints heavy
decide false  # Skips forcing heavy entirely
```

### Property / Law Cheat Sheet

Informal property-style laws validated by representative tests (not exhaustive generators):

| Family | Law | Sample Instance (Stdlib syntax) |
|--------|-----|---------------------------------|
| Maybe Functor | `maybeMap I = I` | `alphaEq (maybeMap I (just 5)) (just 5)` |
| Maybe Applicative | Identity | `maybeAp (just I) v == v` |
| Maybe Applicative | Homomorphism | `maybeAp (just f) (just x) == just (f x)` |
| Maybe Applicative | Interchange | `maybeAp u (just y) == maybeAp (just (λf.f y)) u` |
| Maybe Applicative | Composition | See lhs/rhs example above |
| Maybe Monad | Left Identity | `maybeBind (just x) f == f x` |
| Maybe Monad | Right Identity | `maybeBind m just == m` |
| Maybe Monad | Associativity | `maybeBind (maybeBind m f) g == maybeBind m (λx.maybeBind (f x) g)` |
| Either Functor | Left preserved | `eitherMap succ (left e) == left e` |
| Either Monad | Left Identity | `eitherBind (right x) f == f x` |
| Either Monad | Right Identity | `eitherBind m right == m` |
| Either Monad | Associativity | (see Maybe associativity analog) |
| Applicative Either | Identity/Hom/Interchange | Mirror Maybe laws (Right path) |
| Tracing | Identity preservation | `alphaEq (tap "lbl" 5) 5` |
| Delayed Tracing | No premature forcing | `tapd` avoids forcing untaken branch |
| Show Helpers | Deterministic tag | `head (showBool true) = 116` |

Note: Laws involving arbitrary functions are instantiated with simple numeric functions (`succ`, `mult 2`, etc.) in the test suite for determinism.

## Advanced Usage

This section demonstrates sophisticated applications combining multiple features of the interpreter for real-world functional programming patterns.

### Complex Function Composition and Pipelines

```lambda
# Building sophisticated data processing pipelines (helpers shown all exist)
processNumbers =
    filter (_ > 5) .          # narrow early
    map (square . succ) .     # transform (x+1)^2
    filter (_ < 101)          # emulate upper bound (takeWhile not in stdlib)

processNumbers [1,2,3,4,5,6,7,8,9,10]
# → [49, 64, 81, 100]

# Alternative left-to-right style using the pipeline operator
[1,2,3,4,5,6,7,8,9,10]
    |> filter (_ > 5)
    |> map (square . succ)
    |> filter (_ < 101)

# Combining macros with pipelines
:macro (between $x $low $high) => (and (geq $x $low) (leq $x $high))
:macro (clamp $min $max $x) => (if (lt $x $min) $min (if (gt $x $max) $max $x))

processData = 
    map (clamp 0 100) .
    filter (between _ 10 90) .
    map (_ |> mult 2 |> succ)

processData [-5, 15, 25, 105, 75]  # → [31, 51, 151] after clamping, filtering, transforming
```

> Earlier drafts referenced `takeWhile`; it is **not implemented** in the distributed stdlib. Substitute a final `filter` (as above) or define your own helper.

### Advanced Macro Patterns

#### Conceptual DSL (Illustrative – Not in Stdlib)

The following macros reference imaginary helpers (`record`, `get`, `sortBy`, `groupWith`, `fromList`, `lookup`). They are *not provided*; they show how you could layer a DSL on top.

```lambda
:macro (from $xs) => $xs
:macro (where $p) => (filter $p)
:macro (select $f) => (map $f)
# :macro (orderBy $k) => (sortBy $k)        # needs sortBy implementation
# :macro (groupBy $k) => (groupWith (eq . $k))

# Hypothetical usage once primitives exist:
# people = [ (record "name" "Alice" "age" 30)
#          , (record "name" "Bob"   "age" 25) ]
# from people |> where (λp. gt (get "age" p) 25) |> select (λp. get "name" p)
```

To make this concrete, choose a representation (e.g. association lists of key/value string–number pairs) and implement the missing primitives.

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

#### Structural Equivalence

The interpreter provides an always-on suite of structural comparison helpers: `alphaEq`, `betaEq`, `hashEq`, `etaEq`. These remain active even when `:native off` disables arithmetic fast paths. See detailed table below in the alphaEq subsection.

#### Alpha Equivalence (alphaEq)

`alphaEq` is a native helper that returns a Church boolean (`true` / `false`) indicating whether two expressions are *semantically equal up to normalization and alpha-equivalence*.

> Always-On Structural Equivalence Suite
>
> The interpreter ships with a family of structural comparison helpers that are **always enabled**, independent of the `:native on|off` setting (which only affects arithmetic & comparison speedups):
>
> | Helper  | Purpose |
> |---------|---------|
> | `alphaEq a b` | Normalize both expressions; compare modulo binder renaming |
> | `betaEq a b`  | Beta-normalize both; then alpha compare (skips raw pre-normalization shortcut) |
> | `hashEq a b`  | Beta-normalize; compare canonical De Bruijn structural hash (fast approximation) |
> | `etaEq a b`   | Beta-normalize; eta-reduce; then alpha compare (captures more extensional equalities) |
>
> Use `hashEq` for a quick (possibly conservative) equality screen, and fall back to `betaEq` / `etaEq` for definitive logical equivalence checks depending on whether eta-extensionality matters.

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

## Native List Primitives

The interpreter provides a set of optional native (host-implemented) list helpers for performance. They are enabled/disabled together with arithmetic via `:native on|off` (structural equality natives remain always-on). Each native attempts a fast-path structural pattern-match on a concrete `cons`/`nil` chain. If the argument does not match the expected shape or any element fails conversion (for numeric folds), the native returns `null` so that a pure lambda version (from `stdlib.lambda`) can run instead.

| Name | Aliases | Arity | Behavior | Notes |
|------|---------|-------|----------|-------|
| `length xs` | – | 1 | Returns Church numeral length | O(n) eager traversal |
| `append a b` | `concat` | 2 | Concatenate two lists | Rebuilds new list (no mutation) |
| `reverse xs` | – | 1 | Reverse list | Uses accumulator (O(n)) |
| `map f xs` | – | 2 | Apply `f` to each element | Eager; may allocate new list |
| `filter p xs` | – | 2 | Keep elements where `p x` is Church true | Predicate coerced via Church boolean shape (λt.λf.t/f) |
| `take n xs` | – | 2 | First `n` elements | If `n` > length returns whole list |
| `drop n xs` | – | 2 | Remove first `n` elements | If `n` > length returns `nil` |
| `any p xs` | – | 2 | Church true if any element passes | Short-circuits on first true |
| `all p xs` | – | 2 | Church true if all pass | Short-circuits on first false |
| `find p xs` | `findFirst` | 2 | Returns `(just x)` for first match else `nothing` | Uses maybe constructors from stdlib |
| `sum xs` | – | 1 | Sum of numeric elements | Fallback if any non-numeric element |
| `product xs` | – | 1 | Product of elements (empty => 1) | Early-exits on 0 |

Failure / Fallback (native returns null -> pure stdlib variant used):

- Non `cons`/`nil` structure encountered
- Arity mismatch (pre-checked)
- For `map`/`filter`/`any`/`all`/`find`: predicate result not a Church boolean (shape probe fails)
- For `sum`/`product`: element not a Church numeral

Design Rationale: Returning null instead of throwing keeps semantics extensible—user code can shadow or augment behavior with alternative representations (e.g., Church lists, lazy streams) without losing functionality.

Inspection: `:env native` and `:native show` enumerate these under category `list` with aliases and doc.

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

### Structural Equality Helper Families

For standard encodings you can use lighter-weight structural comparators instead of full normalization:

| Helper | Purpose | Informal Signature |
|--------|---------|--------------------|
| `listEq` | Deep list equality | `(eqE -> list a -> list a -> Bool)` |
| `pairEq` | Pair equality | `(eqA -> eqB -> pair a b -> pair a b -> Bool)` |
| `maybeEq` | Maybe equality | `(eqA -> Maybe a -> Maybe a -> Bool)` |
| `eitherEq` | Either equality | `(eqL -> eqR -> Either l r -> Either l r -> Bool)` |

Macro wrappers (`testList`, `testPair`, `testMaybe`, `testEither`, and *By variants) delegate to these.

#### Lazy Helpers & Benchmarking

| Function | Description |
|----------|-------------|
| `delay`  | Wrap a value into a thunk `(λf.f value)` |
| `force`  | Force a thunk: `force (delay x) = x` |
| `benchmark n f x` | Apply `f` to `x` `n` times (simple micro benchmark) |
| `memoize` | Currently identity (placeholder for future caching layer) |

#### State Monad (Encapsulated State)

State is encoded as functions `s -> pair value newState`.

| Name | Meaning |
|------|---------|
| `returnState` | Lift pure value |
| `bindState` | Monadic bind / sequencing |
| `getState` | Read current state |
| `putState` | Replace state (returns unit-like `nil`) |
| `runState` | Execute a stateful computation |

Example:

```lambda
increment = λs.pair (succ s) (succ s)
runState increment 5      # → pair 6 6
```

#### Loop Combinator (WHILE)

`WHILE cond body initial` repeatedly applies `body` while `cond current` is true.

```lambda
countDown = WHILE (λn.gt n 0) pred 5   # → 0
```

#### Additional Numeric / Utility Functions

| Function | Description |
|----------|-------------|
| `ackermann` | Fast-growing benchmark function (use small arguments) |
| `clamp min max x` | Bound value to inclusive range |

#### Safe Operation Summary

| Function | Returns | Empty / Error Case |
|----------|---------|--------------------|
| `safehead` | `just x` / `nothing` | Empty → `nothing` |
| `safetail` | `just tail` / `nothing` | Empty → `nothing` |
| `safenth n l` | `just (nth n)` / `nothing` | OOB index → `nothing` |
| `safediv a b` / `safeDiv` | `just q` | `nothing` if divisor = 0 |
| `safeMinimum` | `just min` | Empty → `nothing` |
| `safeMaximum` | `just max` | Empty → `nothing` |
| `safeInit` | list (not Maybe) | Empty or singleton → `nil` |
| `safeInitMaybe` | `just prefix` / `nothing` | Empty or singleton → `nothing` |

Chain with `maybe`, `maybeMap`, or a custom bind pattern.

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

### Documentation Generation (Pandoc)

The build script can generate a styled `readme.html` (and optionally other docs like `help.html`) for the Web UI. This requires **Pandoc** to be installed and available on your `PATH`.

Pandoc is NOT bundled. If it is missing the script will emit an error and skip HTML generation.

Install options (choose one):

Windows (Chocolatey):

```pwsh
choco install pandoc
```

Windows (Winget):

```pwsh
winget install --id JohnMacFarlane.Pandoc -e
```

macOS (Homebrew):

```bash
brew install pandoc
```

Linux (Debian/Ubuntu):

```bash
sudo apt-get update && sudo apt-get install -y pandoc
```

Verify:

```bash
pandoc --version
```

Generation happens automatically during `build.ps1` execution; output is copied into `src-webui/wwwroot/readme.html` (and CSS if present). Customize styling via `readme.css` (expected beside `build.ps1`).

If you change `README.md`, re-run the build script (or manually invoke `pandoc` with the same flags) to refresh the Web UI copy.

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

## Contributing (Guidelines)

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
- Error objects are string-based (structured diagnostics planned).
- Sandboxing (timeouts, memory quotas) not yet enforced—use caution in multi-user setups.
- Pattern matching construct (`match`) not yet implemented (macros approximate use cases).

## License (MIT)

This project is licensed under the MIT License. See `LICENSE` for details.

## Distribution & Packaging (New)

Artifacts planned / available:

- NuGet package: `LambdaCalculus.Interpreter` (core library, CEK evaluator, parser, macros). Install with:
    `dotnet add package LambdaCalculus.Interpreter`
- CLI tool project (`src-cli`) provides interactive REPL (build: `dotnet run --project src-cli`).
- Web host (`src-web` / `src-webui`) serves browser UI.

### Roadmap

- Add GitHub Actions workflow to pack & push NuGet on tagged release.
- Provide minimal JS interop API (evaluate, normalize, stats) for embedding.

## Build & Run Guide

This section summarizes how to build and run each form of the interpreter: core library (NuGet), CLI, Web API, and Web UI.

### Prerequisites

- .NET 8 SDK (required). Optional: .NET 9 preview for multi-target build; ignore preview warning if not needed.
- PowerShell (examples assume Windows `pwsh`).
- (Optional) Docker Desktop for container build of Web UI.

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

### 5. Unified Build Script (`build.ps1`)

Examples:

```powershell
# Build everything + pack NuGet + build Docker image
./build.ps1 -Pack

# Skip Docker
./build.ps1 -Pack -NoDocker

# Validate package content & abort if missing files
./build.ps1 -Pack -Validate

# Continue pack even if validation fails
./build.ps1 -Pack -Validate -SkipPackOnError
```

Outputs:

- NuGet package → `artifacts/*.nupkg`
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

### 9. Quick Command Reference

| Target | Command |
|--------|---------|
| Build all | `dotnet build -c Release` |
| Pack | `dotnet pack src/lambda-cek.csproj -c Release -o artifacts` |
| CLI REPL | `dotnet run --project src-cli/lambda-cek.cli.csproj` |
| Web UI | `dotnet run --project src-webui/lambda-cek.webui.csproj` |
| Web API | `dotnet run --project src-web/lambda-cek.web.csproj` |
| Docker image | `docker build -t lambda-cek-webui -f src-webui/Dockerfile .` |

---
