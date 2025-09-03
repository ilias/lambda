# REPL Commands & Operational Guide

This document catalogs all colon (`:`) commands, their effects, related debugging / performance features, and structural equivalence tooling.

Sections
- Command Categories
- Effects & Desugarings Summary
- Full Command Reference Table
- Step Tracing & Debugging
- Performance Statistics & Caches
- Structural Equality / Test Counters
- Practical Workflows

---

### 1. Command Categories

Environment
```
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
```
:lazy on|off            # Lazy (default) vs eager evaluation
:native on|off|show     # Toggle numeric & list native fast paths / list natives
:pretty on|off          # Pretty printing of Church encodings
:step on|off            # CEK step trace output
:depth [n]              # Get/set recursion depth guard (10–10000)
```

Debug / Performance / Logging
```
:stats                  # Performance & cache metrics
:test clear             # Reset structural equality counters
:test result            # Show structural equality counters
:log <file|off|clear>   # Append to file / disable / truncate
```

Language Extension
```
:infix <op> <prec> <assoc>
:macro (pattern) => body
```

Help & Session
```
:help                   # Summary help (abbreviated)
:exit | :quit           # Terminate session
```

---

### 2. Effects & Desugarings Summary

| Construct / Command | Effect / Desugaring |
|---------------------|---------------------|
| `def f x y = body`  | `f = x,y -> body` |
| `x, y -> body`      | `x -> (y -> body)` |
| `let x = A, y = B in C` | Nested lets / lambdas chaining |
| `let rec f = E in B` | `let f = Y (λf.E) in B` |
| `a |> f |> g`       | `g (f a)` |
| `f . a . b`         | `(f a) b` |
| `f ∘ g`             | `λx.f (g x)` |
| `f $ x $ y`         | `f x y` (low precedence) |
| `_` param           | Fresh ignored variable |
| `:infix op p a`     | Registers infix precedence / associativity |
| `:macro (pat) => body` | Adds macro clause (pattern → expansion) |
| `:native on|off`    | Toggle arithmetic & list native fast paths |
| `:clear`            | Full reset (env, macros, infix, stats, caches) |
| `:pretty on|off`    | Toggle pretty printer |
| `:step on|off`      | Toggle CEK tracing |
| `:log file`         | Start appending log lines to file |
| `:log off`          | Stop logging |
| `:log clear`        | Truncate log file |
| `:stats`            | Print runtime statistics |
| `:test result`      | Show structural equality counters |

---

### 3. Full Command Reference Table

| Command | Syntax | Description |
|---------|--------|-------------|
| :clear | `:clear [macros|defs|ops|cache|all]` | Clear portions of interpreter state |
| :depth | `:depth [n]` | Show or set max recursion depth (guard) |
| :env | `:env [defs|macros|infix|native|all]` | Display environment subsets |
| :exit | `:exit | :quit` | Exit interpreter |
| :help | `:help` | Show quick help (not full docs) |
| :infix | `:infix [op prec assoc]` | Define/list infix operators |
| :lazy | `:lazy on|off` | Toggle lazy vs eager evaluation |
| :load | `:load <file>` | Load a .lambda file line-by-line |
| :log | `:log <file|off|clear>` | Manage logging |
| :macro | `:macro (<pattern>) => <body>` | Define macro clause (structural, guarded, rest) |
| :native | `:native on|off|show` | Toggle arithmetic/list natives or list primitives |
| :pretty | `:pretty on|off` | Toggle pretty printing |
| :save | `:save <file>` | Persist current env (defs, macros, infix) |
| :stats | `:stats` | Performance & cache metrics |
| :step | `:step on|off` | CEK reduction tracing |
| :test | `:test clear | :test result` | Structural equality counters |

Tip: To restore a clean baseline without losing macros/infix, use `:clear defs` then `:load stdlib.lambda`.

---

### 4. Step Tracing & Debugging

Enable with `:step on` to emit per‑reduction CEK step lines (`log-step`). Combine with `:pretty off` for raw lambda structures. Typical isolation procedure:

```
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

#### 5a. Performance Metrics Reference (Extended)

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
| Tune laziness | Toggle `:lazy on|off`; inspect `ThunkForceCount` delta |
| Identify hot path | High `TimeInSubstitution` suggests substitution dominates; inline or reduce duplication |

#### 5b. Caching & Memoization Layers

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

### 6. Structural Equality / Test Counters

Helpers always available irrespective of `:native`: `alphaEq`, `betaEq`, `hashEq`, `etaEq`.

Counters: `:test result` prints total calls & successes; `:test clear` resets. Useful in regression to track coverage (e.g., macro correctness via `alphaEq expected actual`).

---

### 7. Practical Workflows

| Goal | Commands Sequence |
|------|-------------------|
| Inspect macro expansion | `:pretty off; :step on` then input expression |
| Benchmark pure function | `:lazy off; :native off; :pretty off; expr; :stats` |
| Restore baseline stdlib | `:clear all; :load stdlib.lambda` |
| Toggle fast arithmetic | `:native on` / `:native off` |
| Debug infinite recursion | `:depth 500; :step on` (watch last frames) |
| Capture session log | `:log session.log` then work; `:log off` |

---

### 8. Multi‑segment Lines

Combine commands and expressions with `;`:
```
:macro (sq $x) => (mult $x $x); sq 9
:load a.lambda; :load b.lambda; :stats
```
Segments processed left→right; errors abort remaining segments.

---

See `LANGUAGE.md` for grammar & syntactic sugar; `COMPILER.md` for building / embedding; `THEORY.md` for background.
