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
- Pretty Printing Rules (Output Representation)

---

### Overview & Philosophy

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
| `a |> f` | Pipeline (left→right data) | `f a` |
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

```
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
```
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
2. Variadic / rest patterns: `$xs ...` (tail capture → Church list of args)
3. Guards with `when (expr)` referencing pattern vars
4. Structural application patterns: `(cons 1 $t)`, `($f $x $y)`
5. Wildcards `_` ignore subexpressions
6. Integer literal patterns match exact numerals
7. Specificity ordering (literal & structural > variable > rest) with tie break on arity then recency
8. Safe placeholder substitution preventing accidental capture (hygiene roadmap)

Examples:
```lambda
:macro (head (cons $h $t)) => $h
:macro (swapArgs ($f $a $b)) => ($f $b $a)
:macro (list $xs ...) => $xs
list 1 2 3  # expands to [1,2,3]
```

Planned enhancements: hygiene / gensym, quasiquote/unquote, macro removal, nested rest, compile‑time eval.

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

---

### See Also

- `COMMANDS.md` for REPL / colon command usage
- `COMPILER.md` for build, packaging, embedding & deployment
- `THEORY.md` for theoretical background
