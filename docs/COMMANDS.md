# REPL Commands & Operational Guide

This document catalogs all colon (`:`) commands, their effects, related debugging / performance features, and structural equivalence tooling.

Sections

- Command Categories
- Effects & Desugarings Summary
- Full Command Reference Table
- Step Tracing & Debugging
- Performance Statistics & Caches
- Native Debug / IO Helpers
- Structural Equality / Test Counters
- Practical Workflows

---

## 1. Command Categories

Environment

```text
:clear                  # Clear env + macros + infix + stats + caches
:clear defs             # Only definitions
:clear macros           # Only macro clauses
:clear ops              # Only custom infix (keeps |> . ∘ $)
:clear cache            # Memoization / analysis caches
:env [defs|macros|infix|native|all]
:load <file>
:save <file>
```

Evaluation / Mode

```text
:lazy on|off            # Lazy (default) vs eager evaluation
:strategy cbv|need      # Select evaluation strategy: call-by-value or call-by-need
:time on|off            # Toggle per-result timing display
:steps                  # Print CEK steps for last evaluation
:binder debruijn on|off # Toggle De Bruijn binder for beta-reduction
:native on|off|show     # Toggle numeric & list native fast paths / list natives
:pretty on|off          # Pretty printing of Church encodings
:step on|off            # CEK step trace output
:depth [n]              # Get/set recursion depth guard (10–10000)
```

Debug / Performance / Logging

```text
:stats                  # Performance & cache metrics
:test clear             # Reset structural equality counters
:test result            # Show structural equality counters
:log <file|off|clear>   # Append to file / disable / truncate
```

Language Extension

```text
:infix <op> <prec> <assoc>
:macro (pattern) => body
# Macros support quasiquote/unquote/splice and are hygienic by default.
# See LANGUAGE.md (Macro System) for syntax and semantics.

Macro tips:

- Use backtick `\`` (or `qq`) to build templates, `~(...)` to insert a single expression, and `~@(...)` to splice multiple args/elements.
- Parenthesize atomic unquotes inside application args: `f ~(x) 1` inserts one argument; `f ~ x 1` parses as `f ~(x 1)`.
- Only use `~@` under quasiquote; if you need to splice a list into a list literal, wrap it in a helper macro like `:macro (spliceList $xs) => (qq [~@ ($xs)])`.
- For mid‑argument construction prefer splicing a list of args: define a helper like `call2` and pass `[$a, $b]` then splice inside the template.
```

Help & Session

```text
:help                   # Summary help (abbreviated)
:exit | :quit           # Terminate session
Documentation & Search

```text
:doc <name>             # Show doc or brief descriptor for symbol
:doc <name> = "text"   # Set/overwrite doc text
:doc export <file>      # Export collected docs to a Markdown file
:find <name>            # Locate where a symbol exists (top/module/macro/native/infix)
:grep <pattern>         # Case-insensitive substring search across names
```

---

### 2. Effects & Desugarings Summary

| Construct / Command | Effect / Desugaring |
|---------------------|---------------------|
| `def f x y = body`  | `f = x,y -> body` |
| `x, y -> body`      | Curried form expansion |
| `let x = A, y = B in C` | Nested lets chaining |
| `let rec f = E in B` | `let f = Y (λf.E) in B` |
| `a \|> f \|> g`       | `g (f a)` pipeline |
| `f . a . b`         | Application chaining |
| `f ∘ g`             | Composition lambda |
| `f $ x $ y`         | Low precedence application |
| `_` param           | Fresh ignored variable |
| `:infix`            | Register infix operator |
| `:macro`            | Add macro clause |
| `:native on/off`    | Toggle numeric/list fast paths |
| `:strategy cbv|need`| Select evaluation strategy (cbv=call-by-value, need=call-by-need) |
| `:time on/off`      | Toggle per-result timing display |
| `:steps`            | Print CEK steps for last evaluation |
| `:binder debruijn on/off` | Enable/disable De Bruijn-based beta-reduction path |
| `:clear`            | Reset interpreter state |
| `:pretty on/off`    | Toggle pretty printer |
| `:step on/off`      | Toggle CEK trace |
| `:log file/off`     | Start or stop logging |
| `:log clear`        | Truncate log file |
| `:stats`            | Runtime statistics |
| `:test result`      | Structural equality counters |

---

### 3. Full Command Reference Table

| Command | Usage | Description |
|---------|-------|-------------|
| :clear  | `:clear macros\|defs\|ops\|cache\|all` | Clear selected state segment |
| :depth  | `:depth n` or `:depth` | Set/show recursion guard |
| :env    | `:env defs\|macros\|infix\|native\|all` | Display environment subset |
| :exit   | `:exit` or `:quit` | Exit interpreter |
| :help   | `:help` | Show quick help |
| :infix  | `:infix op prec assoc` | Define infix operator |
| :lazy   | `:lazy on\|off` | Toggle lazy evaluation |
| :strategy | `:strategy cbv\|need` | Select call-by-value vs call-by-need (lazy) |
| :load   | `:load file` | Load .lambda file |
| :log    | `:log file` / `:log off` / `:log clear` | Manage logging |
| :macro  | `:macro (pattern) => body` | Add macro clause |
| :native | `:native on\|off\|show` | Toggle/list numeric & list natives |
| :binder | `:binder debruijn on\|off` | Toggle De Bruijn binder mode for beta-reduction |
| :time   | `:time on\|off` | Toggle per-result timing display (shown alongside output) |
| :steps  | `:steps` | Print CEK steps performed in last evaluation |
| :pretty | `:pretty on\|off` | Toggle pretty printer |
| :save   | `:save file` | Persist env snapshot |
| :stats  | `:stats` | Performance & cache metrics |
| :step   | `:step on\|off` | Toggle CEK tracing |
| :test   | `:test clear` / `:test result` | Structural equality counters |
| :doc    | `:doc <name>` / `:doc <name> = "text"` / `:doc export <file>` | Show/set/export symbol docs (supports inline `## name: text`) |
| :find   | `:find <name>` | Locate a symbol across top-level, modules, macros, natives, infix |
| :grep   | `:grep <pattern>` | Case-insensitive substring search across names |

Tip: To restore a clean baseline without losing macros/infix, use `:clear defs` then `:load stdlib.lambda`.

---

### 4. Step Tracing & Debugging

Enable with `:step on` to emit per‑reduction CEK step lines (`log-step`). Combine with `:pretty off` for raw lambda structures. Typical isolation procedure:

```lambda
:clear all; :load stdlib.lambda; :step on; :pretty off
<expression>
```

Disable via `:step off` promptly to reduce output overhead.

---

### 5. Performance Statistics & Caches

`:stats` exposes counters (selection):

| Metric | Meaning |
|--------|---------|
| CacheHits / CacheMisses | Evaluation cache effectiveness |
| TimeInSubstitution | Beta-reduction substitution overhead |
| TimeInEvaluation | CEK core loop time |
| TimeInForcing | Lazy thunk forcing cost |
| NormalizeCEKCount | Number of normalization passes |
| Iterations | Machine steps for last eval |
| TotalIterations | Cumulative machine steps |
| ThunkForceCount | Thunks forced total |
| VarCounter | Fresh variable generation counter |

Use cases:

- Compare implementations (e.g., naive vs tail recursion) via `Iterations`.
- Gauge caching by rising hit ratio after repeated calls.
- Detect runaway expansions (surging `Iterations` / large substitution counts).

Reset environment + caches: `:clear cache` (caches only) or full `:clear`.

### 5a. Performance Metrics Reference (Extended)

The `:stats` command exposes fine‑grained timers and counters beyond the abbreviated list above:

| Metric | Meaning |
|--------|---------|
| TimeInCacheLookup | Elapsed ticks spent checking the evaluation cache |
| TimeInSubstitution | Time spent performing variable substitution / beta prep |
| TimeInEvaluation | Core CEK loop time (excludes forcing & cache lookups) |
| TimeInForcing | Time forcing thunks in lazy mode |
| NormalizeCEKCount | Number of top‑level normalization passes |
| CacheHits | Successful evaluation cache lookups |
| CacheMisses | Evaluations that were not cached |
| TotalIterations | Cumulative CEK machine steps this session |
| Iterations | CEK steps for the last evaluation |
| SubstitutionExprCount | Expressions traversed during substitution |
| ThunkForceCount | Thunks forced (realized) total |
| VarCounter | Fresh variable generation counter |
| MaxRecursionDepth | Configured recursion guard limit |

Notes:

- Timings are raw .NET tick counts (convert with `TimeSpan.FromTicks`). Treat them comparatively, not as absolute wall time.
- A low `CacheHits` / high `CacheMisses` ratio indicates little sharing (unique expressions) or cold caches after a reset.
- Rapid growth in `SubstitutionExprCount` or `Iterations` signals heavy beta-reduction; consider refactoring or enabling natives.
- Hitting `MaxRecursionDepth` repeatedly often means missing base cases; increase only after verifying logic.

Workflow patterns:

| Goal | Steps |
|------|-------|
| Measure memoization | Evaluate same expression repeatedly; watch hit ratio climb |
| Compare impls | Run fast & slow versions; contrast `Iterations` / `TimeInEvaluation` |
| Tune laziness | Toggle lazy mode then inspect `ThunkForceCount` change |
| Identify hot path | High `TimeInSubstitution` suggests substitution dominates; inline or reduce duplication |

### 5b. Caching & Memoization Layers

Multiple layers cooperate for performance:

| Layer | Purpose | Key Metric(s) | Clear Command |
|-------|---------|---------------|---------------|
| Substitution cache | Reuse results of repeated variable substitutions | TimeInSubstitution, SubstitutionExprCount | `:clear cache` / `:clear` |
| Evaluation cache | Memoize normalized expression results | CacheHits / CacheMisses | `:clear cache` / `:clear` |
| Free variable cache | Avoid recomputing free variable sets | (indirect) lower substitution cost | `:clear cache` / `:clear` |
| Expression interning | Structural sharing & reduced allocations | (indirect) all timings | `:clear` (full reset) |

Guidelines:

1. Use `:clear cache` before micro-benchmarks to start from a cold state.
2. For pedagogy (pure reductions) keep `:native off` to see genuine lambda costs—then contrast with natives on.
3. High `TimeInCacheLookup` with low hit rate can mean pathological diversity in expressions; ensure common subexpressions are shared or factored.
4. Interning plus alpha-renaming avoidance reduces memory churn; if memory grows unexpectedly, check for large macro expansions.

Future roadmap (not yet implemented): configurable cache size policies & optional per-layer clearing commands.

---

## 6. Native Debug / IO Helpers

Currently exposed native debugging helper:

| Helper | Description |
|--------|-------------|
| `print expr` / `print' expr` | Evaluate & pretty print `expr`. Optional leading label string: `print "lbl" expr` emits `Print lbl <value>`, otherwise `Print <value>`. Returns original value (pipeline friendly) |

Notes:

- Respects `:pretty` mode.
- Forces argument (may realize large lazy structures).
- Logging destination integrates with `:log` if enabled.

Pipeline example:

```lambda
complexCalc |> print |> succ
```

Future additions may include timing wrappers or structured trace output.

---

### 7. Structural Equality / Test Counters

Helpers always available irrespective of `:native`: `alphaEq`, `betaEq`, `hashEq`, `etaEq`.

Counters: `:test result` prints total calls & successes; `:test clear` resets. Useful in regression to track coverage (e.g., macro correctness via `alphaEq expected actual`).

---

### 8. Practical Workflows

| Goal | Commands Sequence |
|------|-------------------|
| Inspect macro expansion | `:pretty off; :step on` then input expression |
| Benchmark pure function | `:lazy off; :native off; :pretty off; expr; :stats` |
| Restore baseline stdlib | `:clear all; :load stdlib.lambda` |
| Toggle fast arithmetic | `:native on` / `:native off` |
| Debug infinite recursion | `:depth 500; :step on` (watch last frames) |
| Capture session log | `:log session.log` then work; `:log off` |

---

### 9. Multi‑segment Lines

Combine commands and expressions with `;`:

```text
:macro (sq $x) => (mult $x $x); sq 9
:load a.lambda; :load b.lambda; :stats
```

Segments processed left→right; errors abort remaining segments.

---

See `LANGUAGE.md` for grammar & syntactic sugar; `COMPILER.md` for building / embedding; `THEORY.md` for background.

---

### Appendix: Non-Interactive Mode (`--no-repl`)

When invoking the CLI you can pass `--no-repl` before any list of `.lambda` files to execute them and terminate without entering the interactive loop. This enables scripting, CI smoke tests, or automated documentation export.

```text
dotnet run --project src-cli -- --no-repl tests/sprint1-smoke.lambda
dotnet run --project src-cli -- --no-repl tests/module-export-hide-smoke.lambda
```

Notes:

- `stdlib.lambda` is loaded automatically first (if present).
- Files are executed in the order given; failures to find a file produce a diagnostic but do not stop later files.
- Combine multiple commands inside a file with `;` separators for batch workflows.

Typical scripted doc export:

```lambda
# script.lambda
:doc export docs/generated-symbol-docs.md
```

Run: `dotnet run --project src-cli -- --no-repl script.lambda`.
