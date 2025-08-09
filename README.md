# Lambda Calculus Interpreter

A high-performance lambda calculus interpreter written in C# featuring lazy evaluation, comprehensive standard library, infix operators, macros, and native arithmetic optimizations.

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
- [Core Language](#core-language)
- [Advanced Syntax Features](#advanced-syntax-features)
- [Interactive Commands](#interactive-commands)
- [Standard Library](#standard-library)
- [Infix Operators](#infix-operators)
- [Macro System](#macro-system)
- [Examples](#examples)
- [Advanced Usage](#advanced-usage)
- [Performance Features](#performance-features)
- [Building and Running](#building-and-running)
- [Range Syntax Extensions](#range-syntax-extensions)
- [Parser Errors & Diagnostics](#parser-errors--diagnostics)
- [Unary Minus / Negative Literals](#unary-minus--negative-literals)
- [Either monad for detailed error reporting](#either-monad-for-detailed-error-reporting)

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
- **Macro System**: Pattern-based macro system for syntactic sugar
- **Native Arithmetic**: Optional native arithmetic optimizations for Church numerals
- **Pretty Printing**: Automatic formatting of Church numerals and lists
- **Comprehensive Standard Library**: Over 200 predefined functions and utilities
- **General & Stepped Ranges**: Rich list range syntax `[a .. b]`, `[a, b .. c]` with lazy dynamic expansion

### Performance Optimizations

- **Memoization**: Multiple caching layers for substitution, evaluation, and free variables
- **Expression Interning**: Memory-efficient expression representation
- **Stack-based Evaluation**: CEK (Control, Environment, Kontinuation) machine for efficient evaluation
- **Thunk Forcing**: Lazy evaluation with intelligent thunk management

## Getting Started

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
:clear                   # Clear environment and caches
:env                     # Show current definitions
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
:memo                    # Clear all caches
:log <file|off>          # Enable/disable logging to file
```

### Language Extensions

```shell
:infix <op> <prec> <assoc>  # Define infix operator
:macro (pattern) => body    # Define macro
:macros                     # List all macros
```

### Help and Information

```shell
:help                    # Show comprehensive help
:multiline               # Show multi-line input help
:native show             # Show all native arithmetic functions
```

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
map (mult 2) [1,3 .. 11]          # [2,6,10,14,18,22]
sum [10,7 .. -5]                  # Handles descending & negative endpoints
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

### Pattern Matching with Let

```lambda
# Simple let binding (syntactic sugar for function application)
let x = 5 in x + 1                                 # → (λx.x + 1) 5

# Multiple bindings (desugared to nested lambdas)
let x = 3, y = 4 in x * y                          # → (λx.λy.x * y) 3 4

# Recursive definitions (uses Y combinator internally)
let rec fib = n -> if (iszero n) 1 (fib (pred n) + fib (pred (pred n))) in fib 10
# → (λfib.fib 10) (Y (λfib.λn.if (iszero n) 1 (fib (pred n) + fib (pred (pred n)))))
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

### 6. Functional Programming Utilities

```lambda
# Function composition and application
compose (mult 2) (plus 3) 5        # 16 ((5+3)*2)
apply (mult 3) 7                   # 21
flip minus 10 3                    # -7 (3-10)

# Currying and partial application
curry (λp.plus (first p) (second p)) 3 4  # 7
partial plus 5 7                   # 12

# Iteration and repetition
iterate (mult 2) 3 16              # 128 (16*2*2*2)
times 4 succ 0                     # 4
```

### 7. String and Character Operations

```lambda
# ASCII character utilities
space, newline, tab               # ASCII constants
isdigit 65                        # false ('A')
isalpha 65                        # true ('A')
tolower 65                        # 97 ('a')
toupper 97                        # 65 ('A')

# String operations (on lists of numbers)
words [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]  # Split on spaces
lines [72, 101, 108, 108, 111, 10, 87, 111, 114, 108, 100]  # Split on newlines
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

## Examples

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
:macro (when $cond $then) => (if $cond $then I)
:macro (unless $cond $then) => (if $cond I $then)
:macro (cond $clauses) => (foldl (\acc clause. if (first clause) (second clause) acc) nil $clauses)

# State machine using macros
:macro (state $name $transitions) => (
    λcurrent input. case current of $transitions
)

# Pattern matching simulation
:macro (match $expr with $patterns) => (
    let value = $expr in $patterns value
)

# Usage example
validateAge = match _ with [
    (between _ 0 12)   -> "child",
    (between _ 13 19)  -> "teen", 
    (between _ 20 64)  -> "adult",
    (_ >= 65)          -> "senior"
]

validateAge 25  # → "adult"
```

### Functional Data Structures

```lambda
# Immutable stack implementation
:macro (makeStack) => nil
:macro (push $stack $item) => (cons $item $stack)
:macro (pop $stack) => (if (isnil $stack) (pair nil nil) (pair (head $stack) (tail $stack)))
:macro (peek $stack) => (if (isnil $stack) nil (head $stack))

# Usage
myStack = makeStack |> push 1 |> push 2 |> push 3
peek myStack        # → 3
top, rest = pop myStack  # → top = 3, rest = [2, 1]

# Functional binary tree
:macro (leaf $value) => (pair $value nil)
:macro (node $value $left $right) => (pair $value (pair $left $right))
:macro (treeValue $tree) => (first $tree)
:macro (treeChildren $tree) => (second $tree)

# Tree traversal with higher-order functions
inorder = Y (λf tree. 
    if (isnil (treeChildren tree))
        [treeValue tree]
        (let children = treeChildren tree in
         append (f (first children)) 
                (cons (treeValue tree) (f (second children)))))

# Create and traverse tree
binaryTree = node 4 (node 2 (leaf 1) (leaf 3)) (node 6 (leaf 5) (leaf 7))
inorder binaryTree  # → [1, 2, 3, 4, 5, 6, 7]
```

### Advanced Recursion Patterns

```lambda
# Mutual recursion using Y combinator
evenOdd = Y (λf. pair 
    (λn. if (iszero n) true (second f (pred n)))
    (λn. if (iszero n) false (first f (pred n))))

isEven = first evenOdd
isOdd = second evenOdd

isEven 42  # → true
isOdd 17   # → true

# Continuation-passing style (CPS)
factorialCPS = Y (λf n k. 
    if (iszero n) 
        (k 1) 
        (f (pred n) (λresult. k (mult n result))))

factorialCPS 5 id  # → 120 (using identity as final continuation)

# Tail-recursive optimization patterns
sumListTR = Y (λf list acc.
    if (isnil list) 
        acc 
        (f (tail list) (plus acc (head list))))

sumList = λlist. sumListTR list 0
sumList [1, 2, 3, 4, 5]  # → 15
```

### Error Handling and Safe Operations

```lambda
# Monadic error handling patterns
:macro (bind $maybe $func) => (
    if (isNothing $maybe) 
        nothing 
        ($func (fromJust $maybe))
)

:macro (safe $operation) => (
    λ...args. try ($operation ...args) catch nothing
)

# Chaining safe operations
safeDivision = λa b. if (iszero b) nothing (just (div a b))
safeLog = λx. if (leq x 0) nothing (just (log x))

computeSafely = λx y.
    bind (safeDivision x y) (λresult.
    bind (safeLog result) (λlogResult.
    just (mult logResult 2)))

computeSafely 100 5  # → just 6.64... (log(20) * 2)
computeSafely 100 0  # → nothing (division by zero)

### Parser Errors & Diagnostics

The parser produces precise diagnostic categories to aid troubleshooting:

| Error | Meaning |
|-------|---------|
| UnexpectedSemicolon | A semicolon appeared where only an expression is allowed (non top-level). |
| UnexpectedLambda    | A lambda parameter list wasn’t followed by a required dot (`λx y.`) before the body. |
| UnexpectedDot       | Misplaced or duplicate dot (e.g., `λx..x` or malformed range). |

Additional Safeguards:
- Lambda parameter lists now strictly require a dot; missing it triggers `UnexpectedLambda`.
- Internal semicolons are disallowed in nested constructs.
- Range syntax validates pattern shapes early for clearer messages.

### Unary Minus / Negative Literals

Negative integers are tokenized directly (no need to write `minus 0 5`). Unary minus is recognized when `-` precedes a number at the start of an expression or after delimiters (`(`, `[`, `=`, `,`, `->`, `;`). Examples:

```lambda
-3 + 5              # 2
[ -2 .. 2 ]         # [-2,-1,0,1,2]
[10,7 .. -5]        # [10,7,4,1,-2,-5]
```

If `-` follows a value (e.g. `x-3`), it’s parsed as subtraction (infix operator) when `-` has been defined via `:infix - <prec> left`.

#### Either monad for detailed error reporting

```lambda
:macro (left $error) => (pair false $error)
:macro (right $value) => (pair true $value)
:macro (bindEither $either $func) => (
    if (first $either)
        ($func (second $either))
        $either
)

parseNumber = λstr.
    if (isdigit (head str))
        (right (parseDigits str))
        (left "Not a number")

validatePositive = λn.
    if (gt n 0)
        (right n)
        (left "Must be positive")

processInput = λstr.
    bindEither (parseNumber str) (λnum.
    bindEither (validatePositive num) (λvalidNum.
    right (mult validNum 2)))
```

### Performance Optimization Techniques

```lambda
# Memoization pattern for expensive recursive functions
createMemoizedFib = 
    let cache = ref emptyMap in
    Y (λf n.
        let cached = lookup n cache in
        if (isJust cached)
            (fromJust cached)
            (let result = if (lt n 2) n (plus (f (pred n)) (f (pred (pred n)))) in
             let _ = insert n result cache in
             result))

memoFib = createMemoizedFib
memoFib 40  # Much faster than naive recursive version

# Lazy evaluation for infinite data structures
:macro (delay $expr) => (λ_. $expr)
:macro (force $thunk) => ($thunk I)

# Infinite list of natural numbers
naturals = Y (λf n. cons n (delay (f (succ n)))) 0
take 10 (force naturals)  # → [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

# Stream processing
fibonacci_stream = Y (λf a b. cons a (delay (f b (plus a b)))) 0 1
take 15 fibonacci_stream  # → [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

# Parallel-style computation simulation
:macro (parallel $computations) => (map force (map delay $computations))

heavyComputation1 = _ -> (factorial 100)
heavyComputation2 = _ -> (fibonacci 30)
heavyComputation3 = _ -> (sum (range 10000))

results = parallel [heavyComputation1, heavyComputation2, heavyComputation3]
```

### Domain-Specific Language Creation

```lambda
# Creating a simple query language
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

### Native Arithmetic

When enabled (`:native on`), the interpreter uses optimized native operations for Church numerals:

```lambda
# These operations are accelerated when native arithmetic is enabled:
plus, minus, mult, div, mod        # Basic arithmetic
succ, pred, iszero                 # Successor/predecessor
lt, leq, eq, geq, gt, neq          # Comparisons
max, min, sqrt, random             # Additional functions
```

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

## Multi-line Input

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

## Building and Running

### Prerequisites

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
