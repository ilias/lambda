# Language Reference

This document describes the surface language, syntax constructs, macro system and core semantic sugar of the Lambda Calculus Interpreter. It is extracted from the original monolithic README for easier navigation.

Sections

- Overview & Philosophy
- Core Constructs & Syntax Cheatsheet
- Examples (Essentials)
- Command / Expression Chaining (Language Feature)
- Placeholders & Multi‑parameter Forms
- Lists & Ranges
- Church Encodings (Integers, Booleans, Pairs, Maybe, Either – forms used by pretty printer)
- Infix Operators (Definition & Precedence)
- Built‑in Special Operators (|>, ∘, ., $)
- Operator Precedence & Associativity Ladder
- Formal Grammar (EBNF‑style)
- Macro System (Basic → Advanced Enhancements)
  - Quasiquote, Unquote, Splice (qq / ~ / ~@)
  - Hygiene (gensym capture-avoidance)
- Pretty Printing Rules (Output Representation)
- REPL Convenience Commands (History / Reload / Last)
- Module System (Aliases, Qualified Names, Import, With-scope)

---

## Overview & Philosophy

The interpreter is a CEK‑machine based lambda calculus environment with pragmatic extensions:

- Pure lambda calculus core (variables, abstraction, application).
- Church encodings for numerals, booleans, lists, pairs, optionals, either.
- Syntax sugar to reduce ceremony (multi‑parameter arrows, `let`, `def`, list literals, ranges, pipelines, application chaining, composition, low‑precedence application).
- A pattern‑driven macro system (multi‑clause, guards, variadic/rest, structural patterns, wildcards, integer literal patterns, precedence heuristics).
- User‑declared infix operators via Pratt parser with dynamic precedence table.

Design goals: expressiveness, readability, hackable experimentation, performance transparency.

---

### Core Constructs & Syntax Cheatsheet

```lambda
λx.x            # Lambda (Alt: \x.x or x -> x)
λx y.x          # Multi-parameter (desugars to λx.λy.x)
x, y, z -> e    # Arrow multi-parameter equivalently nested
def inc x = x+1 # def sugar: inc = x -> x + 1
let id = x->x in id 42          # (λid.id 42) (λx.x)
let x = 3, y = 4 in x * y       # Multi let → nested lets / lambdas
let rec f = n -> ... in f 5     # Recursion via Y combinator desugaring

_ -> 42         # Placeholder (ignored parameter)
(x, _, _) -> x  # Multiple placeholders become fresh uniques
swap = (_, _) -> _ _  # Uses generated vars positionally

[1,2,3]         # cons 1 (cons 2 (cons 3 nil))
[1 .. 5]        # Inclusive literal range
[1,3 .. 11]     # Stepped literal range (step=2)
[f a .. g b]    # Non-literal → (range (f a) (g b)) lazily
[a, a+2 .. b]   # Non-literal stepped → (range2 a (a+2) b)

5 |> succ |> mult 2   # Pipeline (g (f 5))
f . x . y              # Application chaining: (f x) y
f ∘ g                  # Composition λx.f (g x)
f $ x $ y              # Low precedence application

3 + 4 * 5       # Infix (precedence driven)
```

Church numerals: `0 1 2 42` recognized & pretty printed; negative literals `-3` parsed atomically.

---

### Command / Expression Chaining (Language Feature)

Top‑level semicolons separate segments (commands or expressions) processed left→right. Only the final expression result is printed.

```shell
:macro (sq $x) => (mult $x $x); sq 7   # Define then use ⇒ 49
:load stdlib.lambda; plus 2 3          # Chain load + eval ⇒ 5
let x = 10 in succ x; succ 5           # Final result ⇒ 6
```

Rules:

- Semicolons only at REPL/file root (not inside parentheses, lists, lambdas, lets, macros).
- Empty segments ignored; errors abort remaining segments.
- `:exit` / `:quit` stop further segments.

---

### Placeholders & Multi‑parameter Forms

`_` in parameter lists creates a fresh unique variable (ignored unless referenced). Multiple underscores map positionally in the body when used.

```lambda
first = (x, _, _) -> x
swap  = (_, _) -> _ _      # (λa.λb.b a)
map (_ -> 0) [1,2,3]       # Zero out list
addBoth = _ + _ -> mult _ _  # (x,y) -> mult (x + y) (x + y)
```

Underscores improve clarity where some parameters are intentionally dropped.

---

### Lists & Ranges

Literal lists desugar to nested `cons`/`nil` chains. Ranges:

| Form | Expansion |
|------|-----------|
| `[a .. b]` (all integer literals) | Eager expanded list |
| `[a, b .. c]` literals | Eager stepped list (step = b-a) |
| Non‑literal endpoints | `(range a b)` lazy |
| Non‑literal stepped | `(range2 a b c)` lazy |

Descending & negative endpoints supported. Zero step yields singleton.

---

### Church Encodings (Used by Pretty Printer)

- Numerals: `λf.λx.f^n x`
- Booleans: `true = λa.λb.a`, `false = λa.λb.b`
- Lists (fold right): `λf.λz.f a1 (f a2 (... z))`
- Pairs: `pair a b = λf.f a b`
- Maybe: `just x`, `nothing`
- Either: `left x`, `right y`

Pretty printer detects canonical shapes and renders concise forms; disable via `:pretty off` to inspect raw lambdas.

---

### Infix Operators

Declare: `:infix <symbol> <precedence> <left|right>` then bind the symbol with a definition.

Example:

```lambda
:infix + 6 left
:infix * 7 left
:infix ^ 8 right
plus = λa b. ...
mult = λa b. ...
exp  = λa b. ...
1 + 2 * 3      # precedence ⇒ 1 + (2 * 3)
2 ^ 3 ^ 2      # right associative ⇒ 2 ^ (3 ^ 2)
```

Built‑ins always present: `|>`, `∘`, `.`, `$` (their semantics shouldn’t be overridden).

---

### Built‑in Special Operators

| Operator | Meaning | Desugaring |
|----------|---------|------------|
| `a \|> f` | Pipeline (left→right data) | `f a` |
| `f . x`  | Application chaining       | `(f x)` (chains right‑assoc) |
| `f ∘ g`  | Function composition       | `λx.f (g x)` |
| `f $ x`  | Low precedence application | `f x` (right‑assoc; removes parentheses) |

Usage examples:

```lambda
5 |> succ |> mult 2
f . g . 5          # f (g 5)
(f ∘ g) x          # f (g x)
mult 2 $ succ 5
```

---

### Operator Precedence & Associativity

Loosest → tightest (conceptual ladder):

1. Top‑level `;` (segment separator)
2. `let … in …` (right‑assoc)
3. Arrow `p1, p2 -> body` (right‑assoc)
4. Infix operators (numeric precedence: higher = tighter)
5. Special operators tier (pipeline / low‑prec / chaining / composition as declared) – note `|>` & `$` low; `.` & `∘` high
6. Juxtaposition (application) – left‑assoc, tighter than any infix
7. Atom (parenthesized expr, literal, identifier, lambda, list)

Guidelines:

- Application binds tighter than any infix. `f x + g y` ⇒ `plus (f x) (g y)`.
- `$` and `|>` share lowest precedence but opposite associativity.
- Parentheses trump precedence; add them when mixing special operators.

---

### Formal Grammar (Condensed)

EBNF‑style (simplified from full README; Pratt handles infix dynamically):

```BNF
Program      ::= Segment (';' Segment)*
Segment      ::= (Command | Definition | Expression)?
Definition   ::= Identifier '=' Expression
Expression   ::= LetExpr | ArrowExpr | InfixExpr
LetExpr      ::= 'let' ('rec')? LetBinding (',' LetBinding)* 'in' Expression
LetBinding   ::= Identifier '=' Expression | Identifier ParamList '->' Expression
ArrowExpr    ::= ParamList '->' Expression
ParamList    ::= Param (',' Param)* | '(' Param (',' Param)* ')'
Param        ::= Identifier | '_'
InfixExpr    ::= Application (InfixOp Application)*
Application  ::= Atom+
Atom         ::= Integer | Identifier | Lambda | List | '(' Expression ')'
Lambda       ::= ('λ' | '\\') Param+ '.' Expression
List         ::= '[' ListBody? ']'
ListBody     ::= Elements | RangeSpec | SteppedRange
RangeSpec    ::= Expression '..' Expression
SteppedRange ::= Expression ',' Expression '..' Expression
```

Desugarings (informal):

```lambda
let x = A in B           ≡ (λx.B) A
let x = A, y = B in C    ≡ (λx.λy.C) A B
let rec f = E in B       ≡ (λf.B) (Y (λf.E))
x, y -> R                ≡ x -> (y -> R)
[a,b,c]                  ≡ cons a (cons b (cons c nil))
a |> f |> g              ≡ g (f a)
f . a . b                ≡ (f a) b
f ∘ g                    ≡ λx.f (g x)
f $ x $ y                ≡ f x y
```

Parser error taxonomy includes: `UnexpectedToken`, `MissingLetEquals`, `UnexpectedArrow`, `UnexpectedComma`, `UnexpectedSemicolon`, `IllegalAssignment`, `UnexpectedDot`, `UnterminatedList`, `MacroPatternError`.

---

### REPL Convenience Commands

In addition to the core commands (`:load`, `:env`, `:macro`, etc.), the REPL offers quality‑of‑life commands for interactive workflows:

| Command | Syntax | Description |
|---------|--------|-------------|
| `:hist` | `:hist [n]` | Show the last `n` (default 20) entered non‑command top‑level inputs |
| `:repeat` | `:repeat <index\|-k>` | Re-run a prior history entry by absolute index or negative offset (`-1` = last) |
| `:reload` | `:reload` | Reload the most recently loaded file (via `:load`) |
| `:last` | `:last` | Show the last evaluated expression value again |

Details:

- History excludes commands (except ones embedded in multi‑segment lines) and collapses consecutive duplicate entries.
- `:repeat -3` replays the third most recent history entry; the replayed output is prefixed with a `# repeat[index]:` line.
- `:reload` remembers only the most recent `:load <file>` path; if the file was deleted, a warning is shown.
- `:last` prints the stored pretty‑printed value (respecting the current `:pretty` mode). If no expression has been evaluated yet it reports an empty state.
- History is in‑memory only (not persisted across sessions).

These commands are also listed in `:help` and the main `README.md` command table.

---

## Module System

Modules let you load `.lambda` files into an isolated environment and expose their bindings under a qualified alias. This avoids accidental name clashes and allows explicit imports.

### Basics

Qualified access uses `ALIAS::name`.

```lambda
:module load "mylib.lambda" as M
M::foo
M::bar 1 2
```

List all modules:

```lambda
:module list
```

Reload from disk (re-process the file and refresh published symbols):

```lambda
:module reload M
``;

Unload (remove published qualified symbols and registry entry):

```lambda
:module unload M
```

### Aliases and Qualified Names

Set a new registry alias key (note: previously published qualified names keep their original alias; alias changes are registry-level):

```lambda
:module alias OLD as NEW
```

### Import into Unqualified Scope

Selective imports copy module values into the unqualified scope; imported names persist even if the module is unloaded later.

```lambda
:module import M::{foo, bar as barM}
foo 1
barM 2
```

### Temporary With-scope

Evaluate a single expression with all module bindings available unqualified. This is a command form, not an expression—use it at top level.

```lambda
:module with M => (foo 1)
```

### Clear Imported Names

Remove previously imported unqualified names heuristically (qualified symbols remain untouched):

```lambda
:module clear-imports
```

### Name Rewriting & Semantics

- When a file is loaded as a module, the interpreter analyzes which names the file defines and rewrites only free references to those names to their qualified form `ALIAS::name`.
- Bound variables (lambda parameters, let-bound names) are not qualified.
- References to names not defined by the module (e.g., stdlib) are left unqualified and resolve in the host environment.
- Qualified names are published into the main environment for use by other files/REPL inputs.

These rules ensure module internals are self-consistent and do not accidentally capture or shadow host bindings.

### Examples

Assume `m.lambda`:

```lambda
a = 41
b = succ a
f = x -> plus x a
h = plus -> plus 1 2   # bound var stays local
```

Usage:

```lambda
:module load "m.lambda" as M
M::a            # 41
M::b            # 42
(M::f 1)        # 42
M::h            # λplus.3

:module import M::{f as f1, a}
f1 1            # 42
plus a 1        # 42

:module with M => (plus a 1)  # evaluates to 42 (command form)

:module unload M

### Submodules (Hierarchical Aliases)

Module files can load their own submodules using `:module load` inside the file. When a parent module is loaded as alias `A`, any submodule loaded inside (e.g., `:module load "B.lambda" as B`) is registered as a hierarchical alias `A.B`, and its qualified names are available as `A::B::name`.

Rules and behaviors:

- Paths inside module files resolve relative to that file’s directory.
- Dotted aliases are accepted by `:module import` and `:module with`:
  - `:module import A.B::{x as xB, inc}`
  - `:module with A.B => (inc 41)`
- `:module unload A` unloads `A` and all its hierarchical children (`A.B`, `A.B.C`, …). Imported unqualified names persist.
- Cyclic module loads are detected and reported.

Example structure:

```lambda
# submodA.lambda
:module load "submodB.lambda" as B
a = 1
ax = plus a B::x     # becomes A::a and A::B::x when loaded as A

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
:module unload A
```

```lambda
f1 1            # still 42 (import persists)
```

Limitations:

- `:module with` cannot be embedded inside an expression; it is a top-level command that evaluates and prints the result.
- Aliasing changes the registry key only; qualified identifiers published earlier retain the original alias text.

### Documentation & Discovery Aids

- Inline docs inside files: lines starting with `## name: text` attach documentation to `name` without affecting evaluation.
- REPL commands:
  - `:doc <name>` shows doc text if available, or a brief descriptor (def/macro/native/infix locations).
  - `:doc <name> = "text"` sets/overwrites a doc entry.
  - `:doc export <file>` writes all collected docs to a Markdown file.
  - `:find <name>` locates where a symbol is defined (top-level, module members, macros, natives, infix).
  - `:grep <pattern>` searches names across defs/modules/macros/natives/infix (case-insensitive substring).

### Module Export/Hide in Source Files

Inside a module source file, you may control which symbols are published when the file is loaded via `:module load` by adding pseudo-directives at top-level:

```lambda
:module export {a, b, subInc}  # Only these names are published (whitelist)
:module hide {internal}        # Names to exclude (blacklist)
```

Rules:

- If an export set is present, only those names are published. The hide set removes names (applies after export filtering).
- These directives are effective only when loading the file as a module; they do not affect plain `:load` into the global scope.
- Submodules loaded from within the file are still available under hierarchical aliases; you can re-expose a submodule binding by binding a parent name to `Sub::name` and including that name in the export set.

### Macro System

Basic form:

```lambda
:macro (square $x) => (mult $x $x)
square 7   # → 49
```

Advanced capabilities:

1. Multi‑clause macros with guards:

```lambda
:macro (max2 $a $b) when (geq $a $b) => $a
:macro (max2 $a $b) => $b
```

1. Variadic / rest patterns: `$xs ...` (tail capture → Church list of args)
2. Guards with `when (expr)` referencing pattern vars
3. Structural application patterns: `(cons 1 $t)`, `($f $x $y)`
4. Wildcards `_` ignore subexpressions
5. Integer literal patterns match exact numerals
6. Specificity ordering (literal & structural > variable > rest) with tie break on arity then recency
7. Hygienic expansion: macro‑introduced binders are gensym‑renamed to avoid capturing call‑site identifiers
8. Quasiquote templates with unquote and splice: backtick quote `` `...``, unquote `~e`, and unquote‑splice `~@e`

Examples:

```lambda
:macro (head (cons $h $t)) => $h
:macro (swapArgs ($f $a $b)) => ($f $b $a)
:macro (list $xs ...) => $xs
list 1 2 3  # expands to [1,2,3]
```

#### Quasiquote, Unquote, and Splicing

Use backtick to build code templates, `~` to evaluate and insert a single expression, and `~@` to splice a sequence of arguments/elements:

```lambda
# Simple unquote inside a template
:macro (incr $x) => `(succ ~(x))
incr 41  # → 42

# Splicing into an application spine
:macro (apply2 $f $xs ...) => `(~(f) ~@($xs))
apply2 add 1 2  # → (add 1 2)

# Splicing into list literals (pretty printed)
:macro (list* $xs ...) => `[~@($xs)]
list* 1 2 3  # → [1,2,3]
```

Semantics summary:

- Application spines: `~e` inserts one argument position; `~@e` splices zero or more arguments produced by `e`.
- List literals (and Church lists): `~@e` flattens elements; `~e` inserts a single element.
- Nested unquotes: after expanding `~e`, the quasiquote walker re‑enters to allow splices produced by that expansion to take effect at the correct depth.
- Atomicity: a plain `~e` inside application arguments is treated as one argument (it is not flattened like splice).

Supported splicing sources:

- Native list literals (`[a, b, c]`) are flattened by `~@`.
- Church lists are also recognized and flattened by `~@` (e.g., `(cons 1 (cons 2 nil))`).

Best practices & pitfalls:

- Parenthesize atomic unquotes in app args: `plus ~(x) 1` inserts one argument; `plus ~ x 1` parses as `plus ~(x 1)`.
- Use `~@` only inside quasiquote. If you need to splice a list outside, wrap with a helper macro such as `:macro (spliceList $xs) => (qq [~@ ($xs)])` then call `spliceList [1,2,3]`.
- Building calls with mid-arguments? Prefer splicing a list of args rather than nested templates, e.g. `:macro (call2 $f [$a, $b]) => (qq (~($f) ~@ ([$a, $b])))` and `:macro (midArg $a $b) => (call2 (add3 0) [$a, $b])`.
- Nested unquotes re-enter the walker: code produced by `~(...)` is re-walked so inner `~@` splices take effect at the correct depth.
- Lists vs apps: `~@` flattens list elements and application spines; `~` inserts exactly one element/arg.
- Modules: quasiquoted symbols resolve exactly as written (qualified names remain qualified). Use `Mod::f` or `A::B::x` inside `qq` if you need specific module bindings.

#### Hygiene model

Macro expansion is hygienic by default:

- Any binders introduced by a macro (e.g., from `λ`/`let`) are replaced with fresh gensym names so they cannot accidentally capture call‑site variables.
- Unquoted regions (`~...` / `~@...`) are treated as call‑site code and are not renamed; free variables coming from the site remain free.
- Variable resolution prefers the current environment, with a safe fallback to top‑level definitions when appropriate, ensuring expanded code refers to the intended bindings.

Practical effect: you can write macros that introduce helpers or temporary variables without worrying about collisions with names at the invocation site.

Notes:

- Macro expansion order respects pattern specificity (more structural/literal patterns match before generic ones). Guards (`when`) are evaluated in match order.
- For macro debugging, turn pretty printing off (`:pretty off`) and optionally enable stepping/logging to inspect the raw expanded forms.
- Unquote precedence tip: `~` binds to the following expression. If you write `plus ~ a 1`, it parses as `plus ~(a 1)`. To insert a site variable as a single argument, parenthesize the unquote: `plus ~(a) 1`.

Guidance:

- Prefer introducing temporary binders inside macros freely—gensym ensures they can't capture call-site variables.
- Keep call-site code inside `~(...)` or `~@(...)` to preserve names and bindings from the invocation site.
- If an expanded identifier seems to resolve unexpectedly, inspect with `:pretty off; :step on` and check for module qualification or shadowing.
- For references intended to target a specific module binding, write them qualified in the template (e.g., `` `(Mod::f ~(x))``).

---

### Pretty Printing Rules

Enabled by default (`:pretty on`). Converts:

| Raw Shape | Display |
|-----------|---------|
| Church numeral λf.λx.f^n x | `n` |
| Church boolean | `true` / `false` |
| cons / nil chain | `[a, b, c]` |
| Church fold list | `[a, b, c]` |
| Thunk (unforced/forced) | `<thunk: …>` / `<forced: …>` |

Negative literals preserved; output truncates after large length with suffix. Disable using `:pretty off` to inspect raw forms (useful for macro debugging & structural comparison).

---

 

### Structural Equivalence Helpers (Always On)

| Helper | Purpose |
|--------|---------|
| `alphaEq a b` | Normalize & alpha‑compare |
| `betaEq a b`  | Beta normalize then alpha compare |
| `hashEq a b`  | Hash based normalized structural check |
| `etaEq a b`   | Beta normalize + eta reduce then alpha compare |

Intended for regression / property style tests (counters via `:test result`).

#### Structural Equivalence Suite (Extended)

These helpers are native and always enabled (independent of `:native` arithmetic toggles):

| Helper | Operation Pipeline | Notes |
|--------|--------------------|-------|
| `alphaEq a b` | Normalize both; alpha-compare | Ignores binder names only |
| `betaEq a b` | Beta-normalize; alpha compare | Skips pre-normalization shortcut |
| `hashEq a b` | Beta-normalize; structural hash compare | Fast approximate screen; fall back to `betaEq` if needed |
| `etaEq a b` | Beta-normalize; eta-reduce; alpha compare | Captures extensional equalities (λx.f x ≡ f) |

Process outline:

1. Force required thunks (lazy only) to expose heads.
2. Perform bounded normalization (beta; limited inlining of known combinators; guarded to avoid runaway expansion).
3. Apply comparison strategy (hash or alpha).

Guidance:

- Prefer `hashEq` for quick negative checks; confirm positives with `betaEq` / `etaEq`.
- Use `etaEq` only when eta-extensionality matters (slightly more work).
- Rising structural test counts (`:test result`) can highlight hot equivalence paths—optimize upstream definitions or add sharing.

Limitations: Not a proof system; normalization bounds may yield conservative `false` on enormous expanding macros. Eta is not applied in `alphaEq` / `betaEq` to keep their semantics predictable.

---

### Selected Extended Examples

```lambda
compose = f -> g -> x -> f (g x)
twice = f -> x -> f (f x)
map (mult 2) [1,2,3,4]           # [2,4,6,8]
5 |> succ |> mult 2              # 12
(mult 2 ∘ succ) 5                # 12
```

For additional standard library functions see `stdlib.lambda`.

### Property / Law Reference (Informal)

Representative law instances (validated by concrete tests) illustrating expected algebraic behavior of helper families:

| Family | Law (Instance Form) |
|--------|---------------------|
| Maybe Functor | `maybeMap I (just x) = just x` |
| Maybe Applicative | `maybeAp (just I) v = v` |
| Maybe Applicative | `maybeAp (just f) (just x) = just (f x)` |
| Maybe Monad | `maybeBind (just x) f = f x` |
| Maybe Monad | `maybeBind m just = m` |
| Maybe Monad | `maybeBind (maybeBind m f) g = maybeBind m (λx.maybeBind (f x) g)` |
| Either Functor | `eitherMap f (left e) = left e` |
| Either Monad | `eitherBind (right x) f = f x` |
| Either Monad | `eitherBind m right = m` |
| Tracing | `alphaEq (tap "lbl" v) v` |
| Delayed Tracing | Untaken branch not forced (semantic preservation) |
| Show Helpers | Head tag stable (`showBool true` begins with 116) |

To extend: introduce property generators (future roadmap) producing random Church numerals & short lists, then fold over these equations using `alphaEq` / `hashEq` for validation.

### Extended Example: Applicative Composition (Maybe)

```lambda
comp = λf g x.f (g x)
fs  = just succ
gs  = just (mult 2)
xs  = just 5
lhs = maybeAp (maybeAp (maybeAp (just comp) fs) gs) xs
rhs = maybeAp fs (maybeAp gs xs)
alphaEq lhs rhs  # true (applicative composition)
```

---

### Extended Runtime Helpers & Patterns (Migrated)

The following helper families were present in the original monolithic documentation and are now consolidated here for completeness. They complement the core language by offering convenience around laziness, benchmarking, state threading, looping and defensive (safe) data access.

#### Lazy Helpers & Benchmarking

| Helper | Purpose |
|--------|---------|
| `delay x` | Wrap a value in a thunk so it is not evaluated until forced |
| `force t` | Force a thunk created by `delay`; `force (delay v) = v` |
| `benchmark n f x` | Apply `f` to `x` `n` times (micro‑benchmark driver) |
| `memoize f` | Placeholder (currently identity) reserved for future user‑level caching |

Usage sketch:

```lambda
:lazy on
th = delay (expensive 42)
force th            # forces once
benchmark 50 (plus 1) 0   # run (plus 1) 50 times
```

Guidance: Prefer using REPL metrics (`:stats`) around a representative single call over huge `n` unless measuring allocation churn. Disable pretty printing & natives (`:pretty off; :native off`) for purist timing.

#### State Monad (Encapsulated State)

Encodes state threading without manual pair plumbing. A stateful computation is a function from state to pair (value newState).

| Helper | Meaning |
|--------|---------|
| `returnState v` | Lift pure value |
| `bindState m k` | Sequence: feed value of `m` into `k` |
| `getState` | Access current state |
| `putState s` | Replace current state (returns unit‑like `nil`) |
| `runState comp init` | Execute with initial state; returns pair result finalState |

Example:

```lambda
increment = bindState getState (λn.putState (succ n))
runState increment 5        # → pair 6 6
```

#### Loop Combinator (WHILE)

| Form | Semantics |
|------|-----------|
| `WHILE cond body init` | Repeatedly apply `body` while `cond current` is true |

```lambda
countDown = WHILE (λn.gt n 0) pred 5   # → 0
```

Useful for expressing primitive iterative processes without explicit recursion noise.

#### Safe Operations (Maybe‑oriented)

Defensive variants returning `just x` / `nothing` instead of diverging or producing undefined data. (Exact names mirror stdlib; casing variants like `safeDiv` may exist.)

| Helper | Example | Result |
|--------|---------|--------|
| `safehead xs` | `safehead [1,2,3]` | `just 1` |
|                | `safehead []` | `nothing` |
| `safediv a b` / `safeDiv` | `safediv 10 2` | `just 5` |
|                          | `safediv 10 0` | `nothing` |
| `safeInit xs` | `safeInit [1,2,3]` | `[1,2]` |
| `safeInitMaybe xs` | `safeInitMaybe [1,2,3]` | `just [1,2]` |
|                    | `safeInitMaybe [1]` | `nothing` |

These pair naturally with pattern macros and pipelines, reducing explicit error branching.

#### IO / Debug & Tracing Helpers

Two layers of lightweight instrumentation exist: the primitive `print` and stdlib trace wrappers/macros that preserve pipeline flow.

| Helper | Prints | Returns | Description |
|--------|--------|---------|-------------|
| `print expr` / `print "lbl" expr` | value (optional label) | value | Primitive debug print (eager) |
| `trace "lbl" x` | label + value | x | Labeled trace wrapper (eager) |
| `traceVal x` | value | x | Value‑only trace (eager) |
| `traceSilent "lbl" x` | nothing | x | No‑op (placeholder) |
| `traceDelay "lbl" (delay expr)` | label + value | value | Lazy-safe label variant (forces thunk inside) |
| `traceDelayVal (delay expr)` | value | value | Lazy-safe value-only variant |
| pipeline `tap` (expr then label) | label + value | expr | Macro → `trace` labeled (eager) |
| pipeline `tapv` (expr) | value | expr | Macro → `traceVal` (eager) |
| pipeline `tapd` (expr then label) | label + value | expr | Macro → `traceDelay` (lazy-safe) |
| pipeline `tapdv` (expr) | value | expr | Macro → `traceDelayVal` (lazy-safe) |

Semantics & behavior:

- All helpers return the original value so they compose inside `|>` chains.
- `print`/`trace`/`traceVal` force their argument (exposes thunks under lazy mode) – use sparingly inside very deep recursive definitions if laziness matters.
- Labels must be a leading string literal for the labeled forms.
- `traceSilent` is convenient for toggling groups of traces without restructuring code (swap `tap` macro expansion temporarily if needed).

Examples:

```lambda
print 5                      # Print 5
print "dbg" (plus 2 3)       # Print dbg 5
factorial 5 |> print |> succ  # Print 120 -> 121
5 |> tap "seed" |> succ       # Print seed 5 -> 6
5 |> tapv |> succ             # Print 5 -> 6
let x = 3 in x |> tap "x" |> square   # Print x 3 -> 9
(trace "mid" (succ 4)) |> succ        # Print mid 5 -> 6
(traceVal (succ 2)) |> succ            # Print 3 -> 4
(traceSilent "off" (exp 2 5)) |> succ  # No output -> 33
```

Disable pretty printing (`:pretty off`) to inspect raw lambda structures instead of canonical numerals/lists/booleans during tracing. Prefer delayed variants when embedding traces in both branches of recursive conditionals to avoid forcing unused work.

#### Show Helpers (Disambiguation)

Because several Church encodings share numeric forms (e.g. `0` vs `false` vs `nil` vs `nothing`), the stdlib provides lightweight "show" helpers that build ASCII lists with tags:

| Helper | Purpose |
|--------|---------|
| `showBool b` | Produces string "true"/"false" |
| `showMaybe showA m` | "Nothing" or "Just " ++ showA x |
| `showEither showL showR e` | "Left " ++ showL l OR "Right " ++ showR r |

Testing trick: compare the head character code (e.g. 74 = 'J' for `Just`).

#### Maybe / Either Monadic Helpers

Chaining support mirrors common FP libraries:

| Family | Core | Applicative | Extra |
|--------|------|-------------|-------|
| Maybe | `maybeBind` / alias `mbind` | `maybeAp` | `maybeMap2`, `maybeReturn` (= `just`) |
| Either | `eitherBind` / alias `ebind` | `eitherAp` | `eitherMap`, `eitherMapLeft`, `eitherMap2`, `eitherReturn` (= `right`) |

Example snippets:

```lambda
maybeBind (just 2) (λx.just (plus x 3))          # just 5
eitherAp (right succ) (right 4)                  # right 5
eitherBind (left "err") (λx.right (plus x 1))   # left "err"
maybeMap2 plus (just 1) (just 2)                 # just 3
```

Performance note: printing very large Church lists or deep normalized expressions incurs traversal cost; keep such traces temporary.

---

---

### See Also

- `COMMANDS.md` for REPL / colon command usage
- `COMPILER.md` for build, packaging, embedding & deployment
- `THEORY.md` for theoretical background
