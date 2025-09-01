# Interpreter Language Guide

Concise, task‑oriented reference to the lambda‑calculus dialect implemented by this interpreter. Progresses from practical usage to formal grammar. Skip to a later section as needed.

## Contents

1. Quick Start
2. Core Constructs
3. Functions & Parameters
4. Binding & Recursion
5. Numbers & Booleans
6. Lists & Ranges
7. Operators (Infix & Special)
8. Precedence Ladder
9. Macros (Essentials)
10. Structural Equivalence Helpers
11. Interpreter Commands (`:`)
12. Common Parser Errors
13. Formal Grammar (Appendix)
14. Cheat Sheet

---

## 1. Quick Start

```lambda
# Simple arithmetic (assuming stdlib loaded)
plus 2 3                 # 5

# Lambda forms
λx.x                     # identity
\x.x                     # backslash alternative
x -> x                   # arrow sugar
x, y -> plus x y         # multi-parameter sugar

# Let bindings
let x = 3, y = 4 in x + y

# Recursion
let rec fact = n -> if (iszero n) 1 (mult n (fact (pred n))) in fact 5

# Lists and ranges
[1,2,3]
[1 .. 5]
[1,3 .. 11]

# Pipeline / application control
5 |> succ |> succ        # 7
succ $ succ 5            # 7 (right-assoc low precedence)

# Composition (if declared) / chaining
f ∘ g                    # λx.f (g x)
f . a . b                # (f a) b (chaining, not composition)
```

---

## 2. Core Constructs

| Concept | Syntax / Form | Notes |
|---------|---------------|-------|
| Variable | `name` | Letters/digits/_/? (not starting with digit) |
| Lambda | `λx.x` / `\x.x` / `x -> x` | Arrow allows comma list: `x, y -> ...` |
| Multi-parameter sugar | `x, y, z -> body` | Desugars to nested lambdas |
| Application | `f a b` | Left-associative `(f a) b` |
| Grouping | `(expr)` | Overrides precedence |
| Definition | `id = expr` | Top-level binding / overwrite |
| Comment | `# ...` | To end of line |

Desugaring examples:

```lambda
x, y -> body        # ≡ x -> (y -> body)
if p a b            # ≡ p a b (Church boolean)
```

---

## 3. Functions & Parameters

Parameter list forms:

| Form | Meaning |
|------|---------|
| `x -> e` | Single parameter lambda |
| `x, y -> e` | Multi-parameter sugar |
| `_ -> e` | Ignored single parameter |
| `x, _, z -> body` | Ignore middle argument |
| `_ + _ -> mult _ _` | Creates fresh variables for each `_` in list order |

Rules:

- Each `_` in the parameter list becomes a fresh unique binder.
- Reusing `_` in the body refers to the corresponding generated binder by position.
- Underscore inside the body (not in parameter list) refers to that generated binder if in scope.

Examples:

```lambda
_ -> 42                  # constant function
(x, _, z -> x + z) 5 9 2 # 5 + 2
_ + _ -> mult _ _        # (a,b) -> mult (a + b) (a + b)
```

---

## 4. Binding & Recursion

Multiple bindings share a single `let` and scope left‑to‑right:

```lambda
let x = 2, y = succ x in plus x y    # y sees x
```

Recursion uses `let rec` (identifier only on LHS):

```lambda
let rec fib = n -> if (iszero n)
	0
	(if (iszero (pred n)) 1 (plus (fib (pred n)) (fib (pred (pred n)))))
in fib 5
```

`let rec` is clearer than the raw fixed-point (`Y`) combinator (still available if defined in stdlib).

---

## 5. Numbers & Booleans

| Surface | Meaning |
|---------|---------|
| `0`, `1`, `42`, `-7` | Church numeral (negative literal is a single token) |
| `true`, `false` | Church booleans (from stdlib) |
| `if p a b` | Desugars to `p a b` |
| Core arithmetic | `succ`, `pred`, `plus`, `mult`, `exp`, `iszero` |

Notes:

- Numerals print in decimal but evaluate as Church encodings.
- Negative literals are atomic; no implicit unary minus function.

---

## 6. Lists & Ranges

List literal: `[e1, e2, e3]`; empty list: `[]` (`nil`).

Stepped / ranged forms (Haskell style):

```lambda
[1 .. 5]          # 1,2,3,4,5
[10 .. 5]         # 10,9,8,7,6,5 (descending)
[1,3 .. 11]       # 1,3,5,7,9,11 (step +2)
[10,7 .. -2]      # 10,7,4,1,-2 (step -3)
[-3 .. 3]         # negatives allowed
```

Non-literal endpoints produce a lazy range call (you must provide `range` / `range2` e.g. via stdlib):

```lambda
[f x .. g y]      # -> range (f x) (g y)
[a, b .. c]       # -> range2 a b c
```

Common helpers: `cons`, `nil`, `head`, `tail`, `length`, `map`, `filter`, `foldl`, `foldr`, `append`, `take`, `drop`, `reverse`, `sum`, `product`.

---

## 7. Operators (Infix & Special)

Declare infix operators (REPL/file command):

```text
:infix <symbol> <precedence:int> <assoc:left|right>
```

Examples:

```text
:infix + 6 left
:infix * 7 left
:infix ^ 8 right
```

Typical custom declarations:

```text
:infix ++ 5 left      # list/sequence append
:infix |> 1 left      # pipeline (already provided in stdlib)
:infix $ 1 right      # low-precedence application (already provided)
:infix ∘ 9 right      # composition (ensure font supports symbol)
:infix .. 8 right     # (if redefining range operator behaviour)
```

Removal / redefinition: re‑issuing `:infix` with the same symbol updates its precedence/associativity for future parses; previous parsed code is unaffected.

Associativity applies among same-precedence operators.

Special built-in symbols (pre-declared or reserved semantics):

- `|>` (pipeline) – left / 1 – a |> f ⇒ f a (forward pipeline)
- `$` (low application) – right / 1 – f $ x $ y ⇒ f x y
- `.` (chaining) – right / 9 – f . a . b ⇒ (f a) b (NOT composition)
- `∘` (composition) – right / 9 – f ∘ g ⇒ λx.f (g x)

Guideline: Avoid `a |> f . g`; it parses as `(f g) a`. Prefer `a |> f |> g` or parenthesize.

---

## 8. Precedence Ladder

Loosest → tightest conceptual order:

1. Segment separator `;`
2. `let` / `let rec`
3. Arrow parameter grouping
4. Infix operators (higher numeric precedence binds tighter)
5. Chaining / composition (`.`, `∘` at 9, right-assoc)
6. Juxtaposition application
7. Atom (identifier, numeral, lambda, list, parenthesized)

Key guidance:

- `$` and `|>` share precedence 1 but differ in associativity.
- Application outranks all infix: `f x + g y` ⇒ `(plus (f x) (g y))`.
- Composition (`∘`) and chaining (`.`) share precedence; both right-associative, but semantics differ.
- Parentheses always override.

---

## 9. Macros (Essentials)

Pattern-driven rewrites applied before evaluation.

Basic clause:

```lambda
:macro (square $x) => (mult $x $x)
```

Guarded multi-clause:

```lambda
:macro (max2 $a $b) when (geq $a $b) => $a
:macro (max2 $a $b) => $b
max2 5 3  # 5
```

Variadic (rest) pattern (captures trailing args as list):

```lambda
:macro (list $xs ...) => $xs
list 1 2 3 4   # [1,2,3,4]
```

Structural / nested application patterns:

```lambda
:macro (head (cons $h $t)) => $h
:macro (swapArgs ($f $a $b)) => ($f $b $a)
```

Wildcards: `_` ignores a subexpression.
Integer literal pattern matches only that numeral.

Specificity ordering: structural > literal > variable; ties break by arity then recency.

Limitations: At most one rest variable and only at the tail; not (yet) permitted inside nested subpatterns.

---

## 10. Structural Equivalence Helpers

Always available (independent of `:native`):

| Helper | Purpose |
|--------|---------|
| `alphaEq a b` | Alpha-equivalence (ignore binder renaming) |
| `betaEq a b`  | Beta-normalize then alpha compare |
| `etaEq a b`   | Beta + eta reduce then alpha compare |
| `hashEq a b`  | Fast structural hash pre-screen |

Use in tests/assertions; they do not alter evaluation semantics.

---

## 11. Interpreter Commands (`:`)

All REPL/file commands start with a colon. Omit optional arguments to display current configuration or a usage summary.

Commands:

- `:help` — show help summary
- `:infix [op prec assoc]` — define (or list if omitted) infix operator (assoc: left / right)
- `:macro (<pattern>) => <body>` — add macro clause (guards, rest `...` supported)
- `:load <file>` — load a `.lambda` file
- `:save <file>` — save current environment snapshot
- `:env [defs|macros|infix|native|all]` — show environment subsets (default all)
- `:clear [macros|defs|ops|cache|all]` — clear specific state (default all)
- `:depth [n]` — get/set max recursion depth (10–10000)
- `:lazy on|off` — toggle lazy evaluation
- `:native on|off|show` — toggle native arithmetic or list native ops
- `:pretty on|off` — toggle pretty printing (lists, numerals, booleans)
- `:log <file|off|clear>` — append log to file, disable, or clear
- `:stats` — display performance & cache statistics
- `:step on|off` — toggle CEK step trace
- `:test clear` / `:test result` — reset or show structural test counters
- `:exit` / `:quit` — exit interpreter

List current infix operators:

```text
:infix
# + 6 left
# * 7 left
# ^ 8 right
# |> 1 left
# $ 1 right
```

Define a new operator:

```text
:infix ++ 5 left
```

Redefining an operator replaces its precedence/associativity for future parses.

---

## 12. Common Parser Errors

| Error | Meaning | Typical Fix |
|-------|---------|-------------|
| UnexpectedToken | Token not valid here | Check prior grouping / missing delimiter |
| MissingLetEquals | Missing `=` in let binding | Insert `=` |
| UnexpectedArrow | `->` outside a lambda / arrow form | Reposition or remove |
| UnexpectedComma | Stray comma | Remove or adjust list/params |
| EmptyExprList | Required element missing | Provide element / remove brackets |
| UnexpectedSemicolon | `;` where not top-level | Remove or lift |
| IllegalAssignment | Left side not identifier | Use identifier name |
| UnexpectedDot | Misused `.` or `..` | Correct range or chaining |
| UnterminatedList | Missing closing `]` | Add `]` |
| MacroPatternError | Pattern or guard invalid | Fix syntax / guard |

---

## 13. Formal Grammar (Appendix)

Simplified (see README for exhaustive form):

```ebnf
Program       ::= Segment (';' Segment)*
Segment       ::= (Command | Definition | Expression)?
Definition    ::= Identifier '=' Expression
Expression    ::= LetExpr | ArrowExpr | InfixExpr
LetExpr       ::= 'let' ('rec')? LetBinding (',' LetBinding)* 'in' Expression
LetBinding    ::= Identifier '=' Expression | Identifier ParamList '->' Expression
ArrowExpr     ::= ParamList '->' Expression
ParamList     ::= Param (',' Param)* | '(' Param (',' Param)* ')'
Param         ::= Identifier | '_'
InfixExpr     ::= Application (InfixOp Application)*
Application   ::= Atom+
Atom          ::= Integer | Identifier | Lambda | List | '(' Expression ')'
Lambda        ::= ('λ' | '\\') Param+ '.' Expression
List          ::= '[' ListBody? ']'
ListBody      ::= Elements | RangeSpec | SteppedRangeSpec
RangeSpec     ::= Expression '..' Expression
SteppedRangeSpec ::= Expression ',' Expression '..' Expression
```

Associativity & precedence resolved dynamically from the runtime infix table (Pratt parser).

---

## 14. Cheat Sheet

Precedence (loose → tight): `;`, let, arrow grouping, infix (by number), chaining/composition (9), application, atom.

Pipelines / application:

```lambda
a |> f |> g        # g (f a)
f . a . b          # (f a) b (chaining)
(f ∘ g) x          # f (g x)
```

Ranges & lists:

```lambda
[1 .. 4]           # 1,2,3,4
[1,3 .. 9]         # 1,3,5,7,9
[f x .. g y]       # range (f x) (g y)
```

Macros:

```lambda
:macro (inc $x) => (succ $x)
inc 4
```

Desugaring rules (informal → core):

```text
x, y, z -> body      => x -> (y -> (z -> body))
if p a b             => p a b                      # Church booleans
_ -> e               => freshVar1 -> e             # underscore becomes generated name
x, _, z -> body      => x -> freshVar2 -> z -> body (with freshVar2 ignored if unused)
_ + _ -> mult _ _    => a -> b -> mult (a + b) (a + b)
[a .. b]             => (eager expansion) OR range a b (if non-literal endpoints)
[a, c .. b]          => (eager stepped expansion) OR range2 a c b
[f x .. g y]         => range (f x) (g y)
[a, b .. c]          => range2 a b c
f . a . b            => (f a) b                    # chaining, not composition
f ∘ g                => λx.f (g x)                 # composition
a |> f |> g          => g (f a)                    # linear left pipeline
f $ x $ y            => f x y                      # right-assoc low application
let x = e1, y = e2 in e3 => (λx. (λy. e3) e2) e1   # sequential let (conceptual)
let rec f = body in e  => (Y (λf. body)) substituted into e (conceptually)
```

Operator associativity (selected):

```text
Pipeline      (|>) left  prec 1
Low apply     ($)  right prec 1
User +         left prec 6  (example)
User *         left prec 7  (example)
User ^         right prec 8 (example exponent)
Chaining       (.)  right prec 9
Composition    (∘) right prec 9
Application         (implicit tighter than any infix)
```

Key evaluation helpers: `alphaEq`, `betaEq`, `etaEq`, `hashEq` (for structural tests).

End of guide. Happy evaluating!
