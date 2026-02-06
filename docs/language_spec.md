# GoML Language Specification (Implementation Snapshot)

This document captures the language features as implemented in the Rust compiler at commit time. It is intended to be exact rather than aspirational; anything not mentioned here should be treated as unsupported.

## Design Goals and Runtime Model
- Statically typed language with Rust-like surface syntax and ML-inspired expressions.
- Garbage-collected at runtime by targeting Go; no ownership or lifetime system.
- Compiles to Go by monomorphizing generic functions and lambda-lifting local functions (no Go generics or Go closures appear in the generated code).
- Concurrency is delegated to Go goroutines via the `go` keyword in GoML.

## Project Layout and Modules
- Every source file starts with `package Name;`. Package names are identifiers (PascalCase or lowercase).
- Packages can import others with `use Package;` and bring trait names into scope with `use Package::Trait;`.
- Project configuration lives in `goml.toml`.
  - Module root: presence of `[module]` table marks the root; `[module].name` is optional.
  - Required `[package].name`; optional `[package].entry` (default `lib.gom`) chooses the package entry file.
  - Sub-packages either provide their own `goml.toml` (without `[module]`) or are discovered from directory structure and `package` declarations.
  - The CLI and LSP walk upward from the current file to find the module root and then resolve packages recursively.

## Types
- Primitive types: `unit`/`()`, `bool`, `int8/16/32/64`, `uint8/16/32/64`, `float32/float64`, `string`, `char`.
- Composite types:
  - Tuples `(T1, T2, ...)`
  - Fixed arrays `[T; N]`
  - Function types `(A, B) -> C`
  - Generic application `Type[Args]` (e.g. `Vec[int32]`, `Result[string, int32]`)
  - Trait objects `dyn Trait`
- Built-in containers: `Vec[T]`, `HashMap[K, V]` (keys require `Hash + Eq`), and `Ref[T]` (mutable reference cell).
- Built-in enums: `Option[T]` and `Result[T, E]`.
- Type variables appear only on top-level items and impl blocks; closures and local bindings are monomorphic.

## Generics and Constraints
- Only top-level functions, structs, enums, traits, and impl blocks accept type parameters; inner functions and closures cannot be generic.
- Trait bounds use `T: A + B` syntax. The `+` operator for bounds is only allowed inside generic parameter lists.
- The compiler monomorphizes generics before emitting Go; there is no runtime generic dispatch.

## Declarations
### Functions
- Top-level functions must declare full type signatures; omitting the return type defaults to `unit`.
- Generic parameters are written in square brackets before the parameter list.
- Local functions are written as closures `|args| expr` or `|| { ... }` and have a single concrete instantiation.
- Extern declarations:
  - `extern "go" "pkg" ["GoName"] goml_name(params) -> Ret` binds to Go symbols.
  - `extern type Name` declares an external opaque type.
  - `#[builtin] extern fn ...` marks runtime-provided builtins.

### Structs and Enums
- `struct Name[T...] { field: Type, ... }` supports generic parameters.
- `enum Name[T...] { Variant, Variant(T1, T2), ... }` supports payloads and generics.
- Constructors are namespaced with `Enum::Variant` to disambiguate.

### Traits and Implementations
- Traits declare methods as free functions where the first parameter is `Self`.
- Impl blocks support inherent methods (`impl Type { ... }`) and trait implementations (`impl Trait for Type { ... }`), both with optional generics and bounds.
- Trait objects `dyn Trait` are permitted when the trait’s receiver is exactly `Self` and `Self` does not appear elsewhere in the signature (current object-safety rule).
- Method call styles:
  - Static dispatch: `value.method(...)` when the trait is in scope via `use Package::Trait`; otherwise use UFCS `Trait::method(value, ...)`.
  - Dynamic dispatch: for `dyn Trait` values use `Trait::method(value, ...)` (member-style calls on `dyn` are not supported).

## Expressions and Patterns
- Literals: booleans, integers (default `int32`, width-specific suffixes select other integer widths), floats (default `float64`, width-specific suffixes select `float32`), strings, and chars.
- Binding: `let pat = expr;` supports patterns with tuples, structs, enum constructors, literals, and `_`.
- Control flow:
  - `if ... else ...` is an expression; branches must agree on type.
  - `while cond { ... }` returns `unit`.
  - `match expr { pat => expr, ... }` tries arms in order; missing coverage for destructuring and enums is reported.
  - `go expr;` spawns a goroutine to evaluate `expr`, returning `unit`.
- Blocks `{ expr1; expr2; ...; last }` evaluate to the last expression (or `unit` if empty).
- Field and tuple access use `value.field` and `value.0`/`value.1` style projections.
- Function calls allow higher-order values; closures capture variables by reference through lambda lifting.

## Built-in Library Surface
- Printing and strings: `print/println` (via `ToString`), `string_print`, `string_println`, `string_len`, `string_get`, `json_escape_string`.
- Numbers and chars: `*_to_string` for each primitive, hash helpers for `Hash`, and `char_to_string`.
- Vectors: `vec_new`, `vec_push`, `vec_get`, `vec_len` plus inherent methods `Vec::new`, `push`, `get`, `len`.
- Hash maps: `hashmap_new/get/set/remove/len/contains` plus inherent methods on `HashMap`. Keys must satisfy `Hash + Eq`; constraints are enforced during type checking.
- References: `ref`, `ref_get`, `ref_set` produce and operate on `Ref[T]`, which compares and hashes by pointed-to value.
- Traits: builtin `Eq`, `Hash`, `ToString` with implementations for primitives, `Ref[T]`, and container helpers.
- Derive attributes: `#[derive(ToString)]`, `#[derive(ToJson)]`, `#[derive(Hash, Eq)]` generate impls for structs and enums.

## External Interoperability
- `extern "go"` bindings map directly to Go packages and symbols; the optional Go symbol name defaults to the GoML identifier when omitted.
- `extern type` declares opaque types supplied by Go code or other runtimes.
- Builtin externs are injected during compilation and do not require user-provided implementations.

## Compilation Pipeline (for reference)
- Frontend lowers `lexer → parser → CST → AST → HIR → typed AST → Core → Mono → Lift → ANF` before emitting Go.
- Lambda lifting removes local functions; monomorphization specializes all generic calls.
- The emitted Go code is then compiled and executed; expect tests compare each IR stage and final Go output.
