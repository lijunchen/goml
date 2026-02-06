# GoML Language Specification

goml is a statically typed language with syntax similar to Rust, but it includes garbage collection (GC) and compiles to Go, so it does not require lifetime annotations and has no ownership system. In essence, it is closer in nature to OCaml or ML (Meta Language).

In goml, top-level functions must have fully explicit type signatures, and only top-level functions support generic type parameters. Generics are denoted using square brackets. Closures can be defined within functions, but they must have a single concrete type — goml does not include let-polymorphism (a.k.a. let-generalization). goml also supports defining traits, as well as enums and structs similar to Rust, and allows user-defined traits.

The generated Go code does not include generics — goml performs monomorphization by instantiating its own generic function calls. The generated code also does not contain Go closures — goml applies lambda lifting by performing lambda lifting (via lambda lifting) on its local functions.

The file extension for goml source files is `.gom`.

## Table of Contents

- [Lexical Structure and Literals](#lexical-structure-and-literals)
- [Bindings and Scope](#bindings-and-scope)
- [Functions and Closures](#functions-and-closures)
- [Structs, Enums, and Fields](#structs-enums-and-fields)
- [Built-in Containers and References](#built-in-containers-and-references)
- [Control Flow and Expressions](#control-flow-and-expressions)
- [Traits and `impl`](#traits-and-impl)
- [Concurrency and Side Effects](#concurrency-and-side-effects)
- [Attributes and Derivation](#attributes-and-derivation)
- [External Interoperability](#external-interoperability)
- [Additional Conventions and Constraints](#additional-conventions-and-constraints)
- [Builtin Types and Traits](#builtin-types-and-traits)

## Lexical Structure and Literals

### Primitive Types

goml provides the following primitive types:

- `bool` - Boolean type
- `unit`/`()` - Unit type
- Integer types: `int8`, `int16`, `int32`, `int64`
- Unsigned integer types: `uint8`, `uint16`, `uint32`, `uint64`
- Floating-point types: `float32`, `float64`
- `string` - String type
- `char` - Character type (Unicode scalar value)

### Literals

goml supports the following literal types:

- Boolean literals: `true`, `false`
- Integer, unsigned, and floating-point literals
- String literals: enclosed in double quotes, supporting escape sequences
- Char literals: enclosed in single quotes, e.g. `'a'`, supporting escapes like `'\n'` and `'\u0041'`

### Characters

- `char` represents a Unicode scalar value and compiles to Go `rune` (an `int32` code point)
- Char literals use single quotes and support standard escape sequences
- `char` implements `ToString`; `c.to_string()` returns a `string` representation of the code point
- Pattern matching supports `char` scrutinees and `char` literal patterns

### Strings

- String concatenation with `+` is supported
- Multiline strings continue lines with leading `\\` and may contain quotes and backslashes (see `062_multiline_string`)

### Tuples and Wildcards

- Tuples `(a, b, c)` group multiple values together
- The wildcard `_` is commonly used in bindings and pattern matching to ignore values

## Bindings and Scope

### Let Bindings

- `let name = expr;` creates a binding
- Allows shadowing of the same name
- Type annotations are supported: `let x: int32 = 1;`
- `let _ = expr;` discards the result

### Pattern Destructuring

`let` supports pattern destructuring with:
- Tuples: `let (a, b, c) = tuple;`
- Struct fields: `let Point { x, y } = point;`
- Enum constructors: `let Some(value) = option;`
- Wildcards: `let (a, _, c) = tuple;`

Patterns can be mixed and nested.

## Functions and Closures

### Top-Level Functions

- Declaration syntax: `fn name(params) -> Ret { ... }`
- Top-level functions must have explicit signatures
- If the return type is omitted, it defaults to `unit`
- Only top-level functions may declare generic parameters using square brackets: `fn id[T](x: T) -> T`

### Generic Parameters and Trait Bounds

- Generic parameters are declared with square brackets: `fn f[T](x: T) -> T`
- Trait bounds can be added per parameter: `fn f[T: A + B + C](x: T) -> ...`
- The `A + B` syntax is only valid in generic bounds (not in type annotations, param/return types, or `dyn`)

### Function Types

- Function types are written as `(A, B) -> C`
- Can be stored in arrays, passed as arguments, or returned from functions

### Closures

- Syntax: `|args| expr` or `|| { ... }`
- Can capture outer variables
- Support multiple levels of nesting
- Can return closures
- Must have a single concrete type (no let-polymorphism)

## Structs, Enums, and Fields

### Structs

- Declaration: `struct Name { field: Type, ... }`
- Construction supports key–value syntax: `Name { field: value, ... }`
- Field shorthand: `Name { x, y }` when variables match field names
- Field access: `value.field`
- Fields can be used for update reconstruction

### Enums

- Declaration: `enum Name { Variant, Variant(T1, T2), ... }`
- Support generics: `enum Option[T] { Some(T), None }`
- Constructors may be uppercase or lowercase
- Namespaced access `Enum::Variant` avoids conflicts

### Pattern Matching

- Struct patterns support field patterns, shorthand, and wildcards
- Enum matching can destructure payloads or match constructors directly
- Missing coverage results in a compile-time error

## Built-in Containers and References

### Fixed-Length Arrays

- Type: `[T; N]`
- Literals: `[1, 2, 3]`
- Accessed via `array_get` and `array_set`

### Mutable References

- Type: `Ref[T]`
- Created with `ref(x)`
- Accessed and updated via `ref_get` and `ref_set`
- Nested references are supported
- `Ref[T]` implements `Eq`/`Hash` by the pointed-to content (`ref_get(self)`), not pointer identity

### Vectors

- Type: `Vec[T]` (built-in growable vectors)
- Operations: `vec_new`, `vec_push`, `vec_get`, `vec_len`

### HashMap

- Type: `HashMap[K, V]`, backed by generated Go runtime code and Go `map` internally
- Operations:
  - `hashmap_new` - create a new HashMap
  - `hashmap_get -> Option[V]` - get a value by key
  - `hashmap_set -> unit` - set a key-value pair
  - `hashmap_remove -> unit` - remove a key
  - `hashmap_len -> int32` - get the number of entries
  - `hashmap_contains -> bool` - check if a key exists
- Key requirements: `K` must implement both `Hash` and `Eq` traits

## Control Flow and Expressions

### If Expressions

- `if ... else ...` is an expression
- Branches may be nested
- Both branches must have the same type

### While Loops

- `while cond { ... }` loops while condition is true
- Returns `unit`

### Operators

- Boolean operators: `!`, `&&`, `||`
- Arithmetic operators: `+`, `-`, `*`, `/`
- Unary negation: `-expr`
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`

### Match Expressions

- Syntax: `match expr { pattern => expr, ... }`
- Patterns are tried in order
- Pattern types:
  - Literals: `1`, `true`, `"hello"`
  - Tuples: `(a, b, c)`
  - Structs: `Point { x, y }`
  - Enums: `Some(value)`, `None`
  - Bindings: `x` (binds to a variable)
  - Wildcards: `_` (matches anything)
- Missing coverage results in an error

## Traits and `impl`

### Trait Definition

- Syntax: `trait T { fn method(Self, ...) -> ...; }`
- Defines an interface that types can implement

### Trait Implementation

- Syntax: `impl Trait for Type { ... }`
- Can implement for specific generic instances
- Example: `impl ToString for Point { ... }`

### Inherent Implementations

- Syntax: `impl Type { ... }`
- Provides associated functions and methods
- Example: `impl Point { fn new(...) -> Point { ... } }`

### Method Invocation

Multiple invocation styles are supported:

1. Method syntax: `value.method(...)`
2. Associated syntax: `Type::method(value, ...)`
3. Trait syntax: `Trait::method(value, ...)`

For trait methods on non-`dyn` values, `x.method(...)` works when the trait is in scope via `use PackageName::Trait` (builtin traits like `Show` are in the prelude). Otherwise, use UFCS like `Trait::method(x, ...)`.

When multiple trait bounds provide the same method name for a type parameter, `x.foo()` is ambiguous and requires UFCS disambiguation (e.g. `A::foo(x)`).

### Trait Objects

- Type: `dyn Trait` - a first-class type for dynamic dispatch
- Coercion: when the expected type is `dyn Trait`, a value of concrete type `T` is implicitly converted if there is a visible `impl Trait for T`
- Calling: `Trait::method(x, ...)` works for both concrete `x: T` (static dispatch) and `x: dyn Trait` (dynamic dispatch)
- Object safety (current): the receiver must be the first parameter and be exactly `Self`; `Self` is not allowed in other parameters or the return type
- Limitations (current):
  - Trait method call via `x.method(...)` is not supported for `dyn Trait` (use `Trait::method(x, ...)`)
  - Pattern matching on `dyn Trait` is not supported
  - `dyn TraitA + TraitB` and explicit `as dyn Trait` syntax are not implemented

## Concurrency and Side Effects

### Concurrent Execution

- `go expr;` starts concurrent execution
- Commonly used with zero-argument closures: `go || { ... };`

### I/O and Debugging

Common built-in functions for I/O and debugging:

- `string_print` - print a string without newline
- `string_println` - print a string with newline
- `_to_string` - convert basic types to strings
- Derived `to_string` method for types with `#[derive(ToString)]`

## Attributes and Derivation

### Derive Attributes

- `#[derive(ToString)]` - automatically generates `to_string` method
- `#[derive(ToJson)]` - automatically generates JSON serialization
- Applicable to structs and enums

### Usage

```goml
#[derive(ToString)]
struct Point {
    x: int32,
    y: int32,
}
```

## External Interoperability

### External Types

- `extern type Name` declares an external type

### Binding Go Symbols

- Syntax: `extern "go" "pkg" ["Func"] name(params) -> Ret`
- `"pkg"` is the Go package path
- Optional explicit identifier in brackets
- Allows calling Go functions from goml code

## Additional Conventions and Constraints

### Type System Constraints

- Top-level functions must have explicit type signatures
- Generics are limited to top-level functions
- Closures must have a single concrete type (no let-polymorphism)
- Generics are expanded via monomorphization

### Memory Management

- The runtime uses garbage collection
- Manual ownership or lifetimes are not required

## Builtin Types and Traits

### Builtin Traits

#### Eq Trait

```goml
trait Eq {
    fn eq(Self, Self) -> bool;
}
```

Used for equality comparison. Required for HashMap keys.

#### Hash Trait

```goml
trait Hash {
    fn hash(Self) -> uint64;
}
```

Used for hashing values. Required for HashMap keys.

#### Show Trait

Built-in trait for displaying values. Automatically available in the prelude.

### Testing and Naming Considerations

- Adding/changing builtins changes the Builtin interface hash, which can break `crates/compiler/tests/expect/cli_commands_test/*`; update via `env UPDATE_EXPECT=1 cargo test`
- Pipeline tests under `crates/compiler/src/tests/pipeline/` must only be updated via `env UPDATE_EXPECT=1 cargo test` (never hand-edit `.cst/.ast/.hir/.tast/.core/.mono/.anf/.go/.out`)
- Avoid defining user traits named `Eq` or `Hash` in tests/examples unless you fully qualify or rename them; the builtins now reserve these names and duplicate impls can surface as "defined in multiple packages"

## Examples

For comprehensive examples of goml code, explore the test materials in:

- Single-file examples: `crates/compiler/src/tests/pipeline/`
- Multi-module examples: `crates/compiler/src/tests/module/`

Each test directory contains `.gom` source files, compiled `.go` outputs, and `.out` files showing program output.

You can also visit the project page at [lijunchen.github.io/goml/](https://lijunchen.github.io/goml/) for a web-browsable format of the test examples.
