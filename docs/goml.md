# GoML Syntax Description

This article describes the GoML syntax and key type rules implemented in the current repository, mainly for use by code agents when generating, modifying, and reviewing `.gom` source code. It is not a future design proposal; if something resembles Rust, Go, or OCaml syntax but is not listed here, assume that GoML does not support it.

GoML is a statically typed language with garbage collection. Its syntax is close to Rust, while its semantics are closer to ML. Source code is compiled into Go without Go generics or Go closures after monomorphization and lambda lifting. GoML has no ownership, borrowing, lifetimes, or manual memory management.

## Code agent must first remember the rules

1. The project source code uses `.gom`; the project root directory uses `goml.toml` to declare the canonical module path.
2. Each source file in the project first writes `package name;`, then writes the file's own `use`, and finally writes the top-level definition.
3. Parameters of top-level functions must have types; the return type is fixed to `unit` when omitted and is not inferred from the function body.
4. Generics use square brackets: `Vec[int32]`, `fn id[T](x: T) -> T`, not `<...>`.
5. Generic calls usually rely on type inference; when explicit type arguments are required, write `id::[int32](1)`, not `id[int32](1)` or Rust's `id::<int32>(1)`.
6. `if` is an expression; when `else` is omitted, the then branch must return `unit`.`match` must be exhaustive.
7. The last semicolon-free expression of a block is the block value; adding a semicolon discards the value.
8. `let` and assignment statements must end with a semicolon. Mutable bindings may be introduced with `let mut pattern` or precisely inside a pattern with `mut name`; the semicolon can be omitted for `if`, `match`, `while`, `loop` and `for` as statements.
9. Enumeration construction uses full names such as `Option::Some(value)`.In patterns, the enum qualifier may be omitted when the matched type determines it, such as `Some(value)` and `None`.
10. For cross-package calls, write `alias::item`.Top-level items, structure fields, and native methods must all be marked with `pub` as required.
11. Before using trait method syntax across packages, import the package and trait with `use alias::Trait;` or a braced import; when in doubt, use UFCS: `Trait::method(value)`.
12. Do not generate `mod`, `crate::`, `self::`, `super::`, root paths `::x`, Rust references, or Go `var` / `:=`. A user `extern fn` is valid only with the typed Go FFI attribute described below.
13. The test function uses `#[test]`, which must have no parameters, no type parameters and return `unit`; the white-box test is placed in `*_test.gom` of the same package, and the black-box test is placed in the `tests/` directory of the package under test.

## minimal program

Single-file compilation allows omission of `package main;`:

```goml
fn main() -> unit {
    println("hello, goml")
}
```

The entry file in the project should explicitly declare the package:

```goml
package main;

fn main() -> unit {
    println("hello, goml")
}
```

`main` cannot have parameters or type parameters.When generating an executable file, the package selected as the entry must be declared as `package main;` and define `fn main()`.It is recommended to let `main` return `unit`.

## Lexical rules

### identifiers and keywords

Ordinary identifiers only use ASCII and are of the form:

```text
[A-Za-z][A-Za-z_0-9]* | _[A-Za-z_0-9]+
```

The single `_` is a wildcard character, not an ordinary variable name.The current syntax enforces the first letter case of name categories:

- Package names, package aliases in `use package as alias`, functions, methods, parameters, local bindings and fields must start with a lowercase letter or `_`; imported item aliases may follow the naming convention of the imported item;
- Structures, enumerations, traits, enumeration variants, generic parameters, and associated types must start with a capital letter;
- Paths retain the appropriate case for the referenced name.

Enumeration construction should use `Enum::Variant`.Patterns may omit `Enum::` because their expected type determines the variant owner.

Common keywords include:

```text
package use as pub fn struct enum trait impl for type const where defer
let mut if else match while loop for in break continue return go dyn
true false unit bool int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64
float32 float64 string char extern
```

`import`, `mod`, `crate`, `super` and `array` are not current keywords, but similar old declarations will receive migration diagnostics.`self` is a special abbreviation for the receiver parameter, and can also be used as a common receiver variable name; `Self` has special meaning in the type position of trait and impl.

The white space is not noticeable.Only line comments from `//` to the end of the line are supported, block comments are not supported.

### literal

| category | Syntax example | Default or description |
| --- | --- | --- |
| unit | `()` | Type is `unit` |
| bool | `true`、`false` | Type is `bool` |
| integer | `0`、`42`、`1_000`、`0b1010`、`0o755`、`0xff` | Determined by context; defaults to `int` when unconstrained |
| floating point number | `1.25`、`1e3`、`2.5e-2` | Determined by context; defaults to `float64` when unconstrained |
| string | `"text"` | Type is `string` |
| raw string | `r"text"`、`r#"text with \"quotes\""#` | Type is `string`; escapes are not processed |
| byte string | `b"text\\n"` | Type is `Vec[byte]`; ASCII contents and byte escapes are supported |
| raw byte string | `br"text"`、`br#"text with "quotes""#` | Type is `Vec[byte]`; escapes are not processed |
| interpolated string | `f"value={value}"` | Type is `string`; embedded values use `ToString` |
| character | `'a'`、`'\n'`、`'\u0041'` | Type is `char`, representing a Unicode scalar value |
| byte | `b'A'`、`b'\n'`、`b'\xFF'` | Type is `byte`, a transparent alias of `uint8` |

Numbers have no type suffix.Integer literals support binary `0b`/`0B`, octal `0o`/`0O`, decimal, and hexadecimal `0x`/`0X` forms.The `_` delimiter can be used between two digits; floating point numbers support `e`/`E` exponent and optional exponent sign.When using a decimal point, there must be digits on both sides of the decimal point.Negative numbers are composed of unary `-` and positive numeric literals.

Unsuffixed numbers can get the width from the context:

```goml
let small: uint8 = 42;
let ratio: float32 = 0.5;
let values: [int16; 3] = [1, 2, 3];
```

Strings support `\"`, `\\`, `\n`, `\r`, `\t`, `\b`, `\f`, `\/` and four-digit `\uXXXX` escaping; characters are escaped using the same set of control characters, and `\'` is used to represent single quotes. Ordinary strings cannot span lines.

Raw strings use `r"..."` or matching hash delimiters such as `r#"..."#` and `r##"..."##`. Their contents may span lines, and backslashes, quotes, braces, and newlines are retained exactly. Escapes and interpolation are not processed. A quote closes the literal only when it is followed by the same number of `#` characters as the opening delimiter:

```goml
let windows_path = r"C:\users\alice";
let quoted = r#"She said "hello"."#;
let multiline = r##"first line
second line"##;
```

Interpolated strings use `f"..."`. Each `{expression}` is evaluated once from left to right and converted with `ToString`; a value that is already `string` is inserted directly. Write `{{` and `}}` for literal braces. Formatting specifications and interpolated multiline strings are not supported:

```goml
let name = "Ada";
let count = 3;
let message = f"{name} has {count} items {{ready}}";
```

Byte literals contain exactly one ASCII byte and support `\'`, `\\`, `\0`, `\b`, `\f`, `\n`, `\r`, `\t`, and two-digit `\xNN` escapes. Non-ASCII contents are rejected.

Byte strings use `b"..."` and have type `Vec[byte]`. Ordinary byte strings accept ASCII contents together with `\"`, `\\`, `\0`, `\b`, `\f`, `\n`, `\r`, `\t`, `\/`, and two-digit `\xNN` escapes. Unicode escapes and non-ASCII source characters are rejected:

```goml
let request = b"GET / HTTP/1.0\r\n\r\n";
let marker = b"\x00\xFF";
```

Raw byte strings use `br"..."` or matching hash delimiters such as `br#"..."#`. Their contents may span lines and are copied as ASCII bytes without escapes or interpolation:

```goml
let path = br"C:\tmp\file";
let quoted = br#"say "hello""#;
```

The standard library type `std::bytes::Bytes` remains a separate buffer abstraction. Use `bytes::Bytes::from_vec(value)` when an API requires it.

Each line of a multiline string begins with two backslashes; the indentation before the mark is removed, the lines are connected with newlines, and the content after the mark is retained as is.At the end, the next line no longer starts with two backslashes, usually written directly `;` or `}`:

```goml
fn poem() -> string {
    let text = \\roses are red
        \\violets are blue
        \\"quotes" need no escaping here
    ;
    text
}
```

## Modules, packages and imports

### `goml.toml`

A project module declares the canonical path in the root directory:

```toml
[module]
path = "alice::myapp"

[build]
target-dir = "_artifact"

[dependencies]
"alice::http" = "1.2.0"
```

Dependency versions must use the strict `X.Y.Z` form. A dependency version is a minimum version requirement resolved using MVS; there is currently no `goml.lock`.

`[build]` can be omitted; `build.target-dir` defaults to `_artifact` under the module root. The manifest value must be a non-empty relative path and cannot contain a `..` segment. `goml check`, `goml build`, `goml run`, and `goml test` can temporarily override it with `--target-dir <path>`; command-line overrides may be relative or absolute. `goml clean` removes the configured target directory. Its optional `--target-dir <path>` override must stay inside the module.

Each module path segment must be non-empty and may contain ASCII letters, digits, `_`, and `-`. The current manifest parser does not reserve `main`, `builtin`, or `std` as module paths. The `[module]` section currently has no `name`, `kind`, `root`, or similar fields.

### Build product catalog

The default product layout is as follows; the standard package paths in the path are expanded by directory level, for example, `alice::myapp::utils` corresponds to `alice/myapp/utils`:

```text
_artifact/
├── check/
│   ├── pkg/<canonical-package-path>/<last-path-segment>.interface
│   └── deps/<owner>/<module>/<version>/pkg/<canonical-package-path>/<last-path-segment>.interface
├── build/
│   ├── pkg/<canonical-package-path>/<last-path-segment>.interface
│   ├── pkg/<canonical-package-path>/<last-path-segment>.core
│   ├── pkg/<entry-package-path>/goml_generated.go
│   └── deps/<owner>/<module>/<version>/pkg/<canonical-package-path>/<last-path-segment>.*
├── test/
│   ├── base/
│   │   ├── pkg/<canonical-package-path>/<last-path-segment>.*
│   │   └── deps/<owner>/<module>/<version>/pkg/<canonical-package-path>/<last-path-segment>.*
│   ├── internal/
│   │   ├── pkg/<canonical-package-path>/<last-path-segment>.*
│   │   ├── goml_generated.go
│   │   ├── tests.json
│   │   └── runner
│   └── external/
│       ├── pkg/<black-box-test-package-path>/<last-path-segment>.*
│       ├── goml_generated.go
│       ├── tests.json
│       └── runner
└── bin/
    ├── <module-name>
    └── <module-relative-entry-path>/<entry-name>
```

`<last-path-segment>` is the last segment of the canonical package path, not the package name declared in the source code; for example, `alice::app::cmd::server` declared as `package main` still uses `server.core` and `server.interface`.

`check` only requires interface artifacts; `build` and `test` also generate Core. Each entry package declared as `package main` generates a Go link file named `goml_generated.go` in its own artifact directory; ordinary library packages do not generate Go link files. Fixed filenames do not trigger Go's special file rules when a package path ends with `_test`, an operating system name, or an architecture name.

Interface and Core artifacts use the deterministic GAF binary container. GAF
payloads are encoded directly from compiler data without an intermediate JSON
value tree.

`for` loops over builtin `range`, `Vec`, and `Slice` values lower directly to indexed loops without allocating iterator closures.

The executable file of the module root entry package is `bin/<module name>`; the nested entry package retains the directory within the module and appends the entry name. For example, the output of `alice::app::cmd::server` is `bin/cmd/server/server`. Production test dependencies are built once under `test/base`; internal and external tests each share one Go entry, manifest, and runner at the corresponding test-kind root. The runner file has the `.exe` suffix on Windows. External dependencies use the same `deps/<owner>/<module>/<version>/pkg/...` structure in the root directory of each stage.

The configured production directory will not participate in package discovery and cannot be a target to be inspected, built, or tested.The `.gitignore` generated by `goml new` contains `/_artifact/` by default; after modifying `build.target-dir`, the project's own `.gitignore` should be modified simultaneously.

### Catalog package

Each directory containing a `.gom` file is a package.All source files in the same directory must declare the same package name:

```goml
package utils;
```

The canonical identity of a package is the module path plus the relative directory path.The package declaration name determines the local name when imported without aliases, but not the global identity of the package.The canonical identity of the root package is the module path.

A typical project could be:

```text
goml.toml
main.gom
utils/utils.gom
```

`utils/utils.gom`：

```goml
package utils;

pub fn message() -> string {
    "hello"
}
```

`main.gom`：

```goml
package main;

use alice::myapp::utils;

fn main() -> unit {
    println(utils::message())
}
```

### `use`

`use` is a file-level declaration and must be placed after `package` and before any top-level items.Imports are not shared between different files.

Imports are not transitive: package A imports package B, which does not allow files using A to automatically see B.Each file must directly import each package it references.

```goml
use alice::http::client;
use alice::http::client as http_client;
```

After importing the package, access its public items through local aliases:

```goml
let request = http_client::new_request();
```

Braced imports bring selected public top-level items directly into the current file. Functions, constants, structs, enums, type aliases, and traits can be imported, and each item may be renamed independently:

```goml
use alice::rendering::api::{Canvas, Color as Paint, Render, render_to_string};

fn describe(canvas: Canvas) -> string {
    render_to_string(canvas)
}
```

The path before the braces is always a package path. Nested braced imports, `self`, glob imports with `*`, enum-variant imports, and subpackage aggregation are not supported. Imported names remain file-local, and normal top-level visibility rules apply.

The following special form also adds a trait from an already loaded package to the method calling scope:

```goml
use alice::rendering::api;
use api::Render;
```

Importing `Render` in a braced list also adds it to the method scope. Afterwards, the specific value can be written as `value.render()`.Even if the trait is not added to the method scope, you can still write a qualified call to `api::Render::render(value)`.

`pub use` re-exports a named package or public item from the current package without changing its identity:

```goml
pub use alice::http::client;
pub use client::{Client, Request, Response};
pub use client::Request as HttpRequest;
```

Downstream code can use `facade::HttpRequest`, import it directly, or bring a re-exported trait into method scope. The compiler resolves every such path back to the original declaration, so constructors, trait implementations, methods, constants, and functions behave exactly as they do at the original path. Re-exported package namespaces also remain usable, for example `facade::client::Request`. Re-export names in one package must be unique. Glob re-exports remain unsupported.

The following path models are not supported:

```text
mod child;
crate::x
self::x
super::x
::rooted::x
```

### visibility

Top-level items are only visible within the package by default.Top-level functions, structures, enumerations and traits are exported using `pub`; structure fields and inherent impl methods are also private by default and need to be marked `pub` separately for cross-package access:

```goml
pub struct Point {
    pub x: int32,
    pub y: int32,
}

pub fn origin() -> Point {
    Point { x: 0, y: 0 }
}

impl Point {
    pub fn sum(self) -> int32 {
        self.x + self.y
    }
}
```

All files in the same package can use private top-level items.The trait impl method inherits the visibility of the trait method and cannot write `pub`.

## type

### Basic types and composite types

| type | Example | illustrate |
| --- | --- | --- |
| unit | `unit` | The only value is `()`; `()` is not a type notation |
| Boolean | `bool` | `true`、`false` |
| raw integer | `int` | Corresponds to the `int` of the target Go platform and is also the default type of integers. |
| signed integer | `int8`、`int16`、`int32`、`int64` | fixed width |
| unsigned integer | `uint`、`uint8`、`uint16`、`uint32`、`uint64` | `uint` has the target Go platform width; the others have fixed widths |
| byte | `byte` | Transparent builtin alias of `uint8` |
| floating point | `float32`、`float64` | IEEE floating point |
| string | `string` | Go string backend |
| character | `char` | Compile to Go `rune` |
| tuple | `(int32, string)` | Tuples in type syntax have at least two elements |
| fixed array | `[int32; 4]` | The length is part of the type |
| function | `(int32, string) -> bool` | parameter type list to return type |
| Generic application | `Option[int32]`、`pkg::Box[string]` | Use square brackets |
| channel | `Channel[int]` | Go channel backend |
| trait object | `dyn Render`、`dyn Iterator[Item = int]` | A single, non-generic dyn-safe trait; associated types must be bound |
| Associative type projection | `I::Item`、`Self::Output` | There must be corresponding trait constraints |

Example of function type:

```goml
let predicate: (int32) -> bool = |value: int32| value > 0;
let combine: (int32, int32) -> int32 = |a, b| a + b;
let action: () -> unit = || println("run");
```

`(value)` in value and pattern is a group, `(value,)` is a single-element tuple.The current type syntax cannot directly annotate single-element tuples: `(T,)` still normalizes to `T`, so the type of such values ​​must be inferred from the local context.

`A -> B -> C` is parsed by right associative analysis.It is recommended to always write function argument lists in parentheses, especially for higher-order functions: `(A) -> (B) -> C` .

The array length must be a non-negative decimal integer in the source code, not a constant expression:

```goml
let pair: [string; 2] = ["left", "right"];
```

The number of array literal elements must match the array type.Empty arrays and empty generic containers usually require type annotations.

### Type aliases

Top-level aliases are transparent and may have type parameters:

```goml
type UserId = uint64;
type Pair[T] = (T, T);
pub type Names = Vec[string];
```

Aliases do not create nominally distinct types and recursive alias cycles are rejected.Public aliases can be referenced across packages.

### Type syntax not currently available

GoML has no Rust references and lifetimes, raw pointers, nullable types, slice literals, or union types.Use `Ref[T]` to express shared variable units, use `Option[T]` to express optional values, and use `Slice[T]` to express read-only continuous views.

`A + B` is only usable as a trait bound or supertrait list. The parser reserves `dyn A + B`, but the type checker deliberately rejects multiple dyn bounds in the current object model.

`Self` is only used in the trait signature and the type position of impl; ordinary top-level functions cannot use `Self` as an implicit type parameter.

## top level definition

The top level of an ordinary source code file should only contain:

- `fn`
- `struct`
- `enum`
- `trait`
- `impl`
- `type`
- `const`

`package` and `use` can only appear at the beginning of a file.The top level cannot write local variables or arbitrary execution statements.

### Constants

Top-level constants require an explicit type and a compile-time expression:

```goml
const base: int32 = 0x20;
pub const answer: int32 = base + 10;
const newline: byte = b'\n';
```

Constant expressions support scalar literals, references to other constants, unary and binary operators, and casts. They may use forward references, but cycles are rejected. The allowed constant types are `bool`, numeric types, `string`, `char`, and `byte`. Public constants are available through package-qualified paths.

### Compile-time evaluation

`comptime { expression }` requires the expression to be evaluated while the current package is compiled. Its static type is the type of the inner expression, and the compiler replaces it with an equivalent ordinary value before Core lowering:

```goml
#[comptime]
fn factorial(value: int) -> int {
    if value < 2 {
        1
    } else {
        value * factorial(value - 1)
    }
}

const six: int = factorial(3);

fn table() -> [int; 4] {
    comptime {
        [factorial(1), factorial(2), factorial(3), factorial(4)]
    }
}
```

`#[comptime]` marks a non-generic free function as compile-time-capable. The function remains callable at runtime. A compile-time call may call only other `#[comptime]` free functions or the `compile_error(string) -> never` intrinsic. The compiler validates the complete body of every marked function, including branches not taken by a particular invocation. Attributes with arguments, duplicate attributes, generic functions, methods, extern functions, and other declarations are rejected.

A top-level constant initializer is an implicit compile-time context, so `const six: int = factorial(3);` and an initializer wrapped in `comptime { ... }` are equivalent. Top-level constants retain their scalar-only restriction. A `comptime` expression in ordinary code may produce a reifiable tuple, fixed array, struct, or enum in addition to `unit`, `bool`, integer, `string`, and `char` values.

Compile-time code may use local bindings and assignment, blocks, `if`, `match`, `while`, `loop`, restricted `for`, `break`, `continue`, `return`, recursion, direct calls, and supported integer casts and operators. A compile-time `for` accepts only a fixed array or the builtin `int` ranges `start..end` and `start..=end`; its source and range endpoints are evaluated once, and its pattern must be irrefutable. The deterministic string methods `len`, `byte_len`, `get`, `byte_get`, `byte_slice`, `is_char_boundary`, `starts_with`, `ends_with`, and `contains` are also available. String indexes and slices use byte offsets and reject invalid UTF-8 character boundaries.

Compile-time code cannot capture a surrounding runtime parameter or local. Closures, indirect calls, generic functions, methods other than the string whitelist, trait or dynamic dispatch, general iterators, floating-point computation, `Ref`, `Vec`, `HashMap`, channels, goroutines, extern calls, host I/O, environment access, time, randomness, network access, general type reflection, arbitrary declaration generation, compile-time parameters, value generics, and type-level computation are not supported. The constrained programmable derive interface described below is the only reflection and code-generation facility.

`compile_error` is accepted only in a `#[comptime]` function, a `comptime` block, or a top-level constant initializer. It terminates compile-time evaluation with its message. If runtime execution of a `#[comptime]` function reaches it, the program traps:

```goml
#[comptime]
fn checked_size(value: int) -> int {
    if value < 0 {
        compile_error("size must be non-negative")
    } else {
        value
    }
}
```

Failures include the compile-time call stack and source locations. Evaluation uses deterministic instruction, call-depth, temporary-value-node, temporary-memory, and final-value-size limits. Exceeding a limit is a compile error; no wall-clock timeout participates in the language semantics. Successful direct calls are memoized within one evaluation. Memoization is not observable because compile-time code has no user-visible side effects.

Public `#[comptime]` functions can be called from another package. The defining package's interface contains verified compile-time IR for the public entry and the private compile-time helpers and constants it reaches. Public compile-time constant values are also exported. A compile-time body or value change affects the interface hash; source formatting, local names, and source locations do not. A downstream package needs only the dependency interface for checking and evaluation. A value containing hidden fields from another package cannot currently be reified.

Compile-time integer evaluation uses the same fixed-width, wrapping representation as generated runtime code. Division by zero, a negative shift count, and an out-of-bounds index fail compilation. Signed minimum divided by `-1` yields the signed minimum, and its remainder is zero. Narrowing casts retain the low bits of the destination width; widening a signed source sign-extends before conversion to the destination signedness. The CTIR target specification is part of its semantic hash. `int` and `uint` currently use the 64-bit Linux amd64 toolchain target width.

### Structure

```goml
struct Point {
    x: int32,
    y: int32,
}

struct Box[T] {
    value: T,
}
```

The type parameter list of structure and enumeration itself does not write bound.Put constraints on functions, traits, or impl that use the type.

Write out the fields during construction, allowing field abbreviations:

```goml
fn make_point(x: int32, y: int32) -> Point {
    Point { x, y }
}
```

Field access uses dot notation:

```goml
let x = point.x;
```

Structure update copies omitted fields from a base value of the same structure type. The `..base` item must be last. Explicit field expressions are evaluated from left to right, followed by the base expression, and every expression is evaluated once:

```goml
let moved = Point {
    x: point.x + 1,
    ..point,
};
```

Structure update is rejected when inaccessible fields prevent construction across a package boundary.

Directly recursive structures will have infinite size and must be recursed through indirect layers such as `Ref` and `Vec`:

```goml
struct Node {
    value: int32,
    next: Option[Ref[Node]],
}
```

### enumerate

```goml
enum Message[T] {
    Quit,
    Value(T),
    Pair(T, T),
    Named {
        value: T,
    },
}
```

Unloaded variants are used directly as values, loaded variants are called like functions:

```goml
let quit: Message[int32] = Message::Quit;
let value: Message[int32] = Message::Value(42);
let pair = Message::Pair("left", "right");
let named = Message::Named { value: 42 };
```

Unloaded generic variants provide no inference clues, usually the expected type is given:

```goml
let none: Option[int32] = Option::None;
```

Loaded constructors are also available as first-class function values:

```goml
let some: (int32) -> Option[int32] = Option::Some;
```

Structural variants use field constructs and field patterns:

```goml
match named {
    Message::Named { value } => value,
    _ => 0,
}
```

Patterns may omit the enum qualifier.When the expected pattern type is `Option[T]`, `Some(value)` and `None` resolve to `Option::Some(value)` and `Option::None`.The same rule applies to unit, tuple-like, and struct-like variants of user enums, including enums imported from another package and enums reached through a type alias.Duplicate variant names in unrelated enums are not ambiguous because the expected enum type selects the owner.Nested patterns are resolved recursively, so `Some(Ok(value))` uses the payload type of `Some` to resolve `Ok`.

## Functions and generics

### top-level function

```goml
fn add(left: int32, right: int32) -> int32 {
    left + right
}

fn log(message: string) {
    println(message)
}
```

The parameter type cannot be omitted.Omitting `-> ...` is equivalent to `-> unit`.The top-level function name must be unique in the same package, and overloading by parameter type is not supported.

### Generic function

```goml
fn identity[T](value: T) -> T {
    value
}

fn render[T: ToString + Eq](value: T) -> string {
    value.to_string()
}
```

Constraints can also be placed in a `where` clause:

```goml
use std::iter;

fn collect_items[T, I: Iterator](iterator: I) -> Vec[T]
where
    I::Item = T,
{
    iter::collect(iterator)
}
```

There are currently two types of `where` predicates:

```text
Type: TraitA + TraitB
TypeA = TypeB
```

Ordinary generic function calls usually infer the type arguments from the argument and expected result types:

```goml
let number: int32 = identity(1);
let text: string = identity("text");
```

Use GoML turbofish when explicit specification is required:

```goml
let number = identity::[int32](1);
let text = identity::[string]("text");
```

Don't write `identity[int32](1)` or Rust's `identity::<int32>(1)`. Generic arguments for an owner type or trait appear before the member name, while arguments owned by a method appear after it:

```goml
let inferred = value.convert(fallback);
let explicit = value.convert::[string](fallback);
let inherent = Box::[int32]::convert::[string](value, fallback);
let trait_call = Convert::[int32]::convert::[string](value, fallback);
```

Top-level functions and methods may introduce their own type parameters. A method's parameters are distinct from the parameters of its enclosing trait or impl and may have their own bounds and `where` predicates. Local named functions do not exist; use closures. Closures do not have generics. Structures, enumerations, traits, and impl blocks can also have type parameters.

GoML monomorphizes generic calls.Recursive generic code must produce a limited number of concrete instances and cannot continually change to a new nested type with each recursive call.

## Blocks, bindings and assignments

### Block values ​​and semicolons

The last semicolon-less expression of the block is the return value:

```goml
fn square(value: int32) -> int32 {
    let result = value * value;
    result
}
```

A block without a tail expression returns `unit`.Adding `;` after an expression turns it into an expression statement and discards the result:

```goml
fn run() -> unit {
    println("first");
    1 + 2;
}
```

`let`, ordinary assignments, and general non-tail expression statements require semicolons.When used as statements and followed by code, `if`, `match`, `while`, `loop` and `for` can omit the semicolon; other expression statements still require semicolons.Functions, structures, enumerations, traits, impl and blocks themselves are not declared with a semicolon after them.

`defer expression;` registers a `unit` expression to run when the current lexical block is left. Deferred expressions run in last-in-first-out order on normal completion and when `return`, `?`, `break`, or `continue` crosses their block. A return or break value is evaluated before cleanup begins. Each loop-body block has its own cleanup stack, so a deferred expression registered during one iteration runs before that iteration exits.

Unlike Go's `defer`, GoML does not evaluate call arguments when the statement is reached. The complete expression is evaluated at block exit, so reads through `Ref` observe the value at cleanup time. A closure body is a separate control-flow scope. Deferred expressions cannot contain `return`, `break`, `continue`, or `?`, and cleanup during an unrecovered runtime panic is not currently guaranteed. The compiler lowers cleanup to ordinary structured control flow and never emits a Go `defer` statement.

```goml
fn work() -> unit {
    let state = Ref::new("open");
    defer println("closing:" + state.get());
    defer println("flush");
    state.set("ready")
}
```

This prints `flush` first, then `closing:ready`.

### `let`, type annotations and shadowing

```goml
let inferred = 42;
let explicit: int64 = 42;
let _ = println("discard explicitly");
let name = "first";
let name = "second";
```

Local variables allow shadowing with the same name.`let _ = expr;` executes explicitly and discards the result.

The left side of `let` can be an irrefutable pattern:

```goml
let (left, right) = pair;
let Point { x, y: vertical } = point;
```

An ordinary `let` cannot use an enumeration or literal pattern that may fail. Use `let ... else` when the failure path must leave the surrounding control flow:

```goml
fn require_value(value: Option[int]) -> int {
    let Some(item) = value else {
        return -1;
    };
    item
}
```

The initializer is evaluated once. The pattern must be refutable, bindings become visible after the statement, and the `else` block cannot use those bindings. The `else` block must diverge with `return`, `break`, `continue`, an infinite `loop`, or another expression of type `never`. Use `if let` or `match` when both paths continue locally.

### `mut` and assignment

```goml
let mut count = 0;
count = count + 1;
```

Destructuring patterns can mark only selected bindings mutable:

```goml
let (mut index, value) = pair;

match state {
    Some(mut count) => {
        count += 1;
    },
    None => (),
};

for mut item in values {
    item += 1;
}
```

`mut` inside a pattern is allowed only on a binding and does not introduce borrowing or reference semantics. The existing `let mut pattern = value;` form remains supported and recursively makes every binding in that pattern mutable. Per-binding `mut` composes with tuple, struct, enum, array, `match`, and `for` patterns.

Ordinary local bindings without `mut` cannot be reassigned.Assignment targets include mutable locals, tuple projections, structure fields, and supported index locations. Compound assignment supports `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, and `>>=`:

```goml
count += 1;
point.x *= 2;
values[index] <<= 1;
```

The target root and each index are evaluated once before the right-hand side. `++` and `--` are not supported. Compound assignment through `HashMap` indexing is not supported because an indexed read returns `Option[V]`; use `get` and `set`, or ordinary indexed assignment.

Array index assignment requires that the root value is a `let mut` local array, or comes from the built-in `Ref.get()`:

```goml
let mut values = [1, 2, 3];
values[1] = 20;

let shared = Ref::new([4, 5]);
shared.get()[0] = 40;
```

`Vec` and `HashMap` are internal mutable containers, so the contents can be modified via methods or indexes even if the binding itself is not `mut`:

```goml
let values: Vec[int32] = Vec::new();
values.push(10);
values[0] = 20;

let counts: HashMap[string, int32] = HashMap::new();
counts["answer"] = 42;
```

`Slice[T]` is a read-only view and cannot be assigned by index.

## Expressions and operators

### basic expression

GoML supports:

- Literals, variables and paths
- Tuple, array, and structure literals
- Block expressions `{ ... }`
- Function call `f(a, b)`
- Field access `value.field`
- Tuple projection `pair.0`, `triple.2`
- Method call `value.method(arg)`
- Index `value[index]`
- Unary and binary operations
- Integer conversion `value as uint32`
- Half-open range expression `start..end`
- `if`、`if let`、`match`、`while`、`while let`、`loop`、`for`
- closure
- `return`, `break`, `continue`, `go` and `?`

A block is an expression in any ordinary expression position. Its last expression without a semicolon supplies the value; a block without a tail expression has type `unit`:

```goml
let answer = {
    let base = 40;
    base + 2
};

return {
    cleanup();
    answer
};
```

In an `if`, `if let`, `while`, `while let`, `for`, or `match` header, the top-level `{` starts the control-flow body. Parenthesize a block expression used as the header value:

```goml
if ({
    prepare();
    ready()
}) {
    run()
}
```

### operator precedence

From low to high:

| Hierarchy | operator | illustrate |
| --- | --- | --- |
| 1 | `..` | Half-open range; cannot be used in chains |
| 2 | ` |  | ` | short circuit logic or |
| 3 | `&&` | short circuit logical AND |
| 4 | `==`、`!=`、`<`、`>`、`<=`、`>=` | Compare; not chainable |
| 5 | `\ | ` | Bitwise OR |
| 6 | `^` | Bitwise XOR |
| 7 | `&` | Bitwise AND |
| 8 | `<<`、`>>` | shift |
| 9 | `+`、`-` | Addition, subtraction, string concatenation |
| 10 | `*`、`/`、`%` | Multiplication, division, remainder |
| 11 | `as` | explicit integer conversion |
| 12 | Call `()`, index `[]`, `?`, member `.` | suffix |
| 13 | One dollar `-`, `!`, `~` | prefix |

The binary operator is left associative, and the function type `->` is right associative.Don’t write comparisons in chains; use combinations of logical operations:

```goml
let inside = lower <= value && value < upper;
```

Since the current combination of calls, `?` , unary operations, and dot notation are not exactly equivalent to Rust, it is safest to explicitly add parentheses when mixing them:

```goml
let negative = -(compute());
let valid = !(predicate());
```

### Operator type rules

- The two numeric operands of `+ - * /` must be of the same concrete numeric type; there is no implicit numeric promotion.
- `%` only accepts integers of the same type.
- `& | ^ ~` only accepts integers, and both sides of binary bitwise operations must be of the same type.Both sides of `<< >>` must be integers and the result type is the same as the left operand.
- `+` also supports `string + string`.
- `&& || !` only accepts `bool`.
- The unary `-` only accepts signed integers or floating point numbers.
- Both sides of the comparison must be of the same type.
- `< > <= >=` supports numeric values, `string` and `char`.
- `== !=` uses the `Eq` trait; the concrete type must have a visible `Eq` implementation.Tuples and fixed arrays recursively support equality comparisons when the elements support `Eq`.
- `as` supports explicit conversions between integer types, and `char as uint32`.`uint32` to `char` should use `char_from_uint32` which returns `Option[char]`.

There are no exponentiation, null coalescing or user-defined operators.

### range expression

`start..end` constructs an incrementing half-open `FnIterator[int]`, and `start..=end` constructs an inclusive iterator. Both can be used directly in `for`:

```goml
for value in 0..10 {
    println(value)
}

for value in 0..=10 {
    println(value)
}
```

Both ends are `int`. A half-open range is empty when `start >= end`; an inclusive range is empty when `start > end` and contains one value when both ends are equal. Each endpoint is evaluated once from left to right. Inclusive iteration does not compute `end + 1`, so the maximum `int` endpoint does not overflow. Range expressions cannot be chained. Open ranges, character ranges, and custom step syntax are not supported. The `..` and `..=` in patterns are a separate range-pattern syntax.

## control flow

### `if`

`if` is always an expression.`else` is required when generating non-`unit` values:

```goml
fn absolute(value: int32) -> int32 {
    if value < 0 {
        -value
    } else {
        value
    }
}
```

Both branches must produce compatible types.Conditions with only side effects can omit `else`; in this case the then branch must produce `unit`:

```goml
if enabled {
    println("enabled")
}
```

`else if` is another `if` in the `else` branch:

```goml
let label = if score > 90 {
    "high"
} else if score > 60 {
    "middle"
} else {
    "low"
};
```

`if let` executes the then branch when the pattern match is successful and restricts the pattern binding to that branch.The matched expression is evaluated only once:

```goml
let number = if let Option::Some(value) = candidate {
    value
} else {
    0
};
```

The `else` of `if let` can also be omitted; in this case the compiler treats the else branch as `()`, so the then branch must produce `unit`:

```goml
if let Option::Some(value) = candidate {
    println(value);
};
```

When you need to get values ​​from two branches, you must write `else` explicitly.

### `match`

```goml
fn unwrap_or(value: Option[int32], fallback: int32) -> int32 {
    match value {
        Some(inner) => inner,
        None => fallback,
    }
}
```

The matched expression is evaluated only once, and the pattern is tried from top to bottom.All branches must produce compatible types, and the compiler is required to cover all possible values.

An unqualified variant in a pattern is resolved only against the matched value's type.The compiler does not search unrelated enums for a fallback.If that type is not an enum, does not contain the variant, or cannot be inferred, the pattern is rejected.Fully qualified patterns remain available:

```goml
match value {
    Option::Some(inner) => inner,
    Option::None => fallback,
}
```

The rule is shared by `match`, `if let`, `while let`, `let`, `for`, nested patterns, and or-patterns.Existing refutability requirements still apply to `let` and `for`.

Branches can be guarded.guard must be a `bool`, and bindings introduced by this branching pattern can be used:

```goml
match value {
    Option::Some(inner) if inner > 0 => inner,
    Option::Some(_) => 0,
    Option::None => -1,
}
```

guard only evaluates after a successful pattern match.Branches with ordinary guards are not counted in exhaustive coverage; only branches without guards and branches with guard literal `true` provide coverage.Branches with guard literal `false` are unreachable.The compiler also warns about branches that are completely covered by earlier patterns.

A branch body can be a single expression or a block.Different branches must be separated by commas, and the comma in the last branch can be omitted:

```goml
match value {
    Option::Some(inner) => {
        let doubled = inner * 2;
        doubled
    },
    Option::None => 0,
}
```

### `while`

```goml
let mut index = 0;
while index < limit {
    index = index + 1;
};
```

The condition must be `bool` and the loop result is `unit`. A `while` loop accepts only `break` without a value.

`while let` evaluates the right-hand expression once in each round; when the match is successful, it enters the loop body and provides pattern binding, and when the match fails, it exits the loop:

```goml
while let Option::Some(value) = iterator.next() {
    println(value);
};
```

The loop body must return `unit`.Pattern binding is only visible within the loop body.

### `loop`

`loop` repeats a block until control leaves it. Unlike `while` and `for`, it is an expression whose result comes from `break`:

```goml
let result = loop {
    let candidate = next();
    if candidate >= 0 {
        break candidate;
    };
};
```

All `break` values targeting the same `loop` must have compatible types. `break;` supplies `unit`, so it cannot be mixed with non-`unit` break values. A `loop` with no `break` targeting it has type `never` and does not continue to the following expression. Unlabeled `break` and `continue` target the nearest loop.

### `for`

```goml
let values: Vec[int32] = Vec::new();
values.push(10);
values.push(20);

for value in values {
    println(value);
};
```

`for pattern in source { ... }` accepts fixed arrays and values ​​that implement `IntoIterator`.Both the source expression and the `into_iter` transformation are executed only once.The pattern must be irrefutable and the loop body must return `unit`.`start..end` can be used directly as a native `int` range.

Tuple destructuring can be used directly in loops:

```goml
for (key, value) in pairs {
    println(key + value.to_string());
};
```

Fixed arrays use a native indexed loop. `Vec[T]`, `Slice[T]` and all `Iterator` values have corresponding `IntoIterator` implementations.

### `break`, `continue` and `return`

```goml
while true {
    if done {
        break
    } else {
        continue
    };
};
```

`break` and `continue` can only appear in loops. `continue` never has a value. `break value` is allowed only for `loop`; `while` and `for` accept `break;` only. Both are divergent control expressions and can appear in `if`, `match`, or other value positions.

Rust-style labels select an enclosing `while`, `loop`, or `for` without conflicting with `break value`:

```goml
'outer: for row in rows {
    for item in row {
        if found(item) {
            break 'outer;
        } else {
            continue;
        }
    }
}

let answer = 'result: loop {
    break 'result 42;
};
```

`continue 'label;` resumes the selected enclosing loop. `break 'label value;` may carry a value only when the selected target is a `loop`. Labels must refer to an active enclosing loop and cannot be duplicated while an outer label of the same name is active. Crossing one or more blocks still runs their `defer` cleanups from inner to outer before control reaches the target.

`return` can be taken with or without a value and checks the return type of the current function or closure:

```goml
fn first_positive(values: Vec[int32]) -> int32 {
    for value in values {
        if value > 0 {
            return value
        } else {
            ()
        };
    };
    -1
}
```

### `?`

The suffix `?` only supports the built-in semantics `Option[T]` and `Result[T, E]`:

```goml
fn plus_one(value: Option[int32]) -> Option[int32] {
    Option::Some(value? + 1)
}

fn read_number(flag: bool) -> Result[int32, string] {
    let value = parse_number(flag)?;
    Result::Ok(value)
}
```

When using `?` with `Option[T]`, the nearest function or closure must return `Option[_]`.Use of `Result[T, E]` must return `Result[_, E]` with the same error type; there is currently no Rust `From`-style error conversion.`?` will evaluate the operand once.

### `go`

`go` starts a zero-argument closure that returns `unit`:

```goml
go || {
    println("background")
};
```

Don't write `go work();`; that will get `unit` first, and the value type required by `go` is `() -> unit`.Should be written `go || work();` .

`go` is a detached, unstructured escape hatch. Its lifetime is not tied to the caller, it does not return a task handle, and its failures are not propagated. Use `std::task` when the caller must wait for child work or coordinate cancellation.

The `unstructured_go` lint warns on each `go` expression. A function or method that intentionally owns detached background work can use `#[allow(unstructured_go)]`.

## model

Patterns can be used with `let`, `for`, `match`, `if let` and `while let`.

| model | Example |
| --- | --- |
| variable binding | `value` |
| Wildcard | `_` |
| unit | `()` |
| bool/number/string/character | `true`、`42`、`"ok"`、`'x'` |
| Group | `(pattern)` |
| tuple | `(left, right)` |
| Single element tuple | `(value,)` |
| exact structure | `Point { x, y: other }` |
| partial structure | `Point { x, .. }` |
| Enum unit variant | `Color::Red` or contextual `Red` |
| Enum tuple variant | `Option::Some(value)` or contextual `Some(value)` |
| Enum struct-like variant | `Message::Named { value }` or contextual `Named { value }` |
| Array, Vec or Slice | `[first, second]`、`[first, .., last]` |
| rest binding | `[first, middle @ .., last]` |
| Alias | `whole @ Option::Some(value)` |
| or-pattern | `Color::Red \| Color::Blue` |
| scope | `0..10`、`'a'..='z'` |
| Nested mode | `Result::Ok((key, value))` |

### Rebuttability and binding

`let` and `for` require that the pattern be irrefutable, that is, the type at that position must succeed.`match`, `if let` and `while let` can use rebuttable patterns:

```goml
let (left, right) = pair;

if let Option::Some(value) = candidate {
    println(value);
};
```

Irrefutability is judged jointly by type and sub-pattern rather than by surface syntax alone.For example, the destructuring of a single-variant enum is irrefutable, as is the complete destructuring of a fixed-length array of known length; [first, ..] of a Vec[T] may fail with an empty Vec.

Variables with the same name cannot be bound repeatedly in the same pattern branch.Each alternative of the or-pattern must be bound to exactly the same set of variables, and the corresponding variables must be of the same type:

```goml
match value {
    Either::Left(shared) | Either::Right(shared) => shared,
}
```

Pattern binding is only visible in the corresponding `let` subsequent scope, `for` / `while let` loop body, `if let` then branch, or `match` branch guard and branch body.

### Groups, tuples, structures and enumerations

`(pattern)` only changes the pattern combination method, `(pattern,)` is the single-element tuple pattern:

```goml
let (only,) = one_tuple;

match candidate {
    (Option::Some(value)) => value,
    Option::None => 0,
}
```

Structure field patterns support abbreviations with the same name:

```goml
let Point { x, y } = point;
```

Struct mode is exact by default and all fields must be listed.Unlisted fields can be ignored using the trailing `..`:

```goml
let Point { x, .. } = point;
```

`..` appears at most once in the structure pattern and must be the last item.Duplicate fields, unknown fields, and missing fields without writing `..` will all generate diagnoses.

The number of constructor parameters of the enumeration pattern must be consistent with the definition.An unqualified variant is resolved from the expected enum type rather than from global uniqueness.For example, `Shared` in a pattern for `First` means `First::Shared` even when `Second::Shared` also exists.If `First` has no `Shared` variant, the compiler reports that error instead of selecting `Second::Shared`.Type aliases and imported enum types participate after normalization.Nested variant payloads supply the expected type for nested patterns.

### Fixed arrays, Vec and Slice

Fixed arrays `[T; N]`, `Vec[T]` and `Slice[T]` share square bracket mode.Without `..`, the pattern requires the exact length; with `..`, the prefix and suffix only specify the minimum length:

```goml
match values {
    [] => "empty",
    [only] => "one",
    [first, .., last] => "many",
}
```

There is at most one rest in a sequence pattern.rest can be located anywhere, or it can be written as `name @ ..` to bind the middle part:

```goml
let values: [int32; 4] = [1, 2, 3, 4];
let [first, middle @ .., last] = values;
```

For fixed arrays, the number of elements must be exactly equal to `N` when rest is omitted; when rest is included, the total number of elements of explicit prefixes and suffixes cannot exceed `N`.In the above example, the type of `middle` is `[int32; 2]`.

For `Vec[T]` and `Slice[T]`, the binding type of `name @ ..` is read-only `Slice[T]`.For example `[head, tail @ ..]` requires at least one element, `tail` will not be copied into a new Vec.Since dynamic sequences may not be long enough, such patterns are usually placed inside a `match`, `if let` or `while let`.

### Alias ​​and or-pattern

`name @ pattern` binds the entire matched value to `name` while continuing to match the inner pattern:

```goml
match value {
    whole @ Option::Some(inner) => use_both(whole, inner),
    Option::None => fallback(),
}
```

`@` is more tightly bound than `|`.Therefore `whole @ A | B` resolves to `(whole @ A) | B`, and usually an error will be reported because the variables bound to the two alternatives are different.To make the alias cover the entire or-pattern, you must write parentheses:

```goml
whole @ (Either::Left(value) | Either::Right(value))
```

or-pattern is tried from left to right and can be nested in tuple, struct, enumeration and sequence patterns.

### range mode

`start..end` does not contain an upper bound, `start..=end` contains an upper bound:

```goml
match character {
    'a'..='z' => "lowercase",
    'A'..='Z' => "uppercase",
    _ => "other",
}
```

Both endpoints must be integer or character literals of the same concrete type.Signed integer endpoints can be negative.The exclusive range requires the lower bound to be strictly less than the upper bound, and the inclusive range requires the lower bound to be less than or equal to the upper bound.Open ranges, floating point ranges, or string ranges are not currently supported; separate `..` in sequences and structures is a rest, not a range.

### Exhaustiveness and unreachable branches

The compiler checks whether `match` is exhausted by the actual type of the pattern, and gives examples of missing patterns if it is not.The analysis covers bool, enumerations, tuples, structures, fixed arrays, Vec/Slice, integer and character ranges, aliases and or-patterns, and will also warn about branches that are never matched.

An empty `match` is only valid for types that have no constructible value:

```goml
enum Never {}

fn absurd(value: Never) -> int32 {
    match value {}
}
```

Exhaustive analysis ignores enumeration variants with no value for the payload type and also identifies purely recursive enumerations with no base variant.For example, `enum MaybeNever { Empty, Filled(Never) }` just overrides `Empty`; `enum Loop { Next(Loop) }` has no constructible value.

Branches with normal guards do not provide exhaustive coverage because guard may be false; subsequent unguarded branches are usually required.String and floating-point literals cannot enumerate the entire type, and `_` is also usually required when matching these types.In floating point mode, `-0.0` and `0.0` are regarded as the same value.

There are no `ref` or `ref mut` patterns. `mut name` is supported for individual bindings, while pattern matching on `dyn Trait` is not supported.

## Closures and function values

### closure syntax

```goml
let add = |left: int32, right: int32| left + right;
let increment = |value: int32| {
    value + 1
};
let greet = || println("hello");
```

Closure parameter types can be inferred from the expected function type or the calling location; when inference is unstable, it should be explicitly marked:

```goml
let transform: (int32) -> string = |value| value.to_string();
```

Closures can capture external variables or modify captured `let mut`:

```goml
let mut count = 0;
let next = || {
    count = count + 1;
    count
};
```

`return` in a closure returns the closure, not the outer function.

### No let-polymorphism

Each closure and local function value has only one concrete type:

```goml
let identity = |value| value;
let number = identity(1);
```

The `string` cannot be processed with the same `identity` thereafter.Define top-level generic functions when polymorphism is required:

```goml
fn identity[T](value: T) -> T {
    value
}
```

Top-level functions, closures, and loaded enumeration constructors can all be passed or returned as function values.
Closures can be nested and return another closure that captures the environment, but each resulting closure expression still has only one concrete function type.

## trait and impl

### Define traits

Trait method signatures must include parameter names.The first receiver can be written as `self`, which is short for `self: Self`:

```goml
trait Render {
    fn render(self) -> string;
}

trait Convert[T] {
    fn convert(self, fallback: T) -> T;
}
```

A trait may also declare an associated function without a receiver. `Self` is inferred from its arguments or the expected return type:

```goml
trait Decode {
    fn decode(input: string) -> Self;
}

trait Default {
    fn default() -> Self;
}

fn decode_value[T: Decode](input: string) -> T {
    Decode::decode(input)
}

fn default_value[T: Default]() -> T {
    Default::default()
}
```

An impl method must preserve whether the trait declaration has a `self: Self` receiver. Associated functions use static dispatch, cannot be called with method syntax, and do not participate in `dyn` dispatch. If neither arguments nor the expected result determine `Self`, the call is rejected as ambiguous.

Traits can have supertraits, generic constraints, and associated types:

```goml
trait Provider: Render {
    type Item: ToString;

    fn get(self) -> Self::Item;
}
```

Generic trait example:

```goml
trait Child[T: ToString]: Parent[T] + Render
where
    T: Eq,
{
    fn child(self) -> string;
}
```

Trait methods may provide a default body. An impl may omit such a method and will inherit the default; an explicitly declared impl method overrides it. Default bodies are checked using the trait's generic parameters, predicates, associated types, and `Self`, and remain available across package boundaries:

```goml
trait Named {
    fn name(self) -> string;

    fn describe(self) -> string {
        "named:" + self.name()
    }
}

impl Named for Point {
    fn name(self) -> string {
        "point"
    }
}
```

Trait methods may also declare their own type parameters and constraints:

```goml
trait Convert {
    fn convert[U: ToString](self, fallback: U) -> string;
}
```

The corresponding impl method must have the same method-generic arity, signature, and constraints. Generic trait methods use static dispatch and make the trait unavailable as `dyn`.

### trait impl

```goml
struct Point {
    x: int32,
    y: int32,
}

impl Render for Point {
    fn render(self: Point) -> string {
        self.x.to_string() + "," + self.y.to_string()
    }
}
```

The type parameters of generic impl are written after `impl`:

```goml
impl[T: ToString] Render for Box[T] {
    fn render(self: Box[T]) -> string {
        self.value.to_string()
    }
}
```

Different applications of generic traits are different impl:

```goml
impl Convert[int32] for Token {
    fn convert(self: Token, fallback: int32) -> int32 {
        7
    }
}

impl Convert[string] for Token {
    fn convert(self: Token, fallback: string) -> string {
        "seven"
    }
}
```

Associated types are bound in impl:

```goml
impl Iterator for Counter {
    type Item = int32;

    fn next(self: Counter) -> Option[int32] {
        next_value(self)
    }
}
```

When implementing a trait with supertraits, you also need to provide the impl of the target type for each supertrait.The compiler rejects overlapping impls and enforces the orphan rule: at least one of the trait or the nominal type being implemented must belong to the current package.The target nominal type of intrinsic impl must belong to the current package.

### intrinsic impl

```goml
impl Point {
    fn new(x: int32, y: int32) -> Self {
        Point { x, y }
    }

    fn sum(self: Self) -> int32 {
        self.x + self.y
    }
}
```

The associated function without a receiver is called with `Point::new(1, 2)`; the method whose first parameter is the receiver can be called with `point.sum()` or `Point::sum(point)`.

Intrinsic impl of generic types:

```goml
impl[T] Box[T] {
    fn get(self: Self) -> T {
        self.value
    }

    fn map[U](self: Self, map_fn: (T) -> U) -> Box[U] {
        Box { value: map_fn(self.value) }
    }
}
```

Method type arguments are normally inferred. Write `box_value.map::[string](convert)` when explicit arguments are needed, or `Box::[int32]::map::[string](box_value, convert)` in associated form.

### Method parsing and UFCS

For specific values, use:

```goml
value.render()
Render::render(value)
```

Cross-package method syntax requires that the trait be in the method scope of the current file:

```goml
use acme::render;
use render::Render;
```

If multiple visible traits define methods with the same name, `value.render()` will be ambiguous.Use UFCS to explicitly select:

```goml
let a = A::render(value);
let b = B::render(value);
```

Generic trait UFCS uses `::[...]` after the trait name to give the type parameters:

```goml
let number = Convert::[int32]::convert(token, 0);
let text = Convert::[string]::convert(token, "");
```

Trait bounds for type parameters make the corresponding methods available for generic values.supertrait and associated type bound will also participate in parsing as implicit constraints.

## `dyn Trait`

Concrete values ​​that satisfy the dyn-safe trait are implicitly boxed when `dyn Trait` is expected:

```goml
trait Display {
    fn show(self) -> string;
}

fn erase[T: Display](value: T) -> dyn Display {
    value
}

fn show_dynamic(value: dyn Display) -> string {
    value.show()
}
```

The `dyn` value supports method syntax, and UFCS can also be used; it can be seen that supertrait methods are also available:

```goml
value.show()
Display::show(value)
```

Associated types are fixed in square brackets and are available to dyn-dispatched method signatures:

```goml
trait Source {
    type Item;

    fn get(self: Self) -> Self::Item;
}

fn read(source: dyn Source[Item = int]) -> int {
    source.get()
}
```

Every associated type declared by the trait must be bound exactly once. The bracket grammar reserves positional trait arguments followed by associated bindings, such as `dyn Consumer[string, Error = IoError]`; positional arguments after the first `Name = Type` binding are rejected. Generic trait objects are still rejected, so the positional form is reserved for forward compatibility rather than enabled today.

Current dyn-safe conditions:

- Traits cannot have type parameters;
- Each method must have a first receiver parameter of exactly type `Self`;
- Direct `Self` cannot appear in other parameters or return types, while a bound projection such as `Self::Item` is allowed;
- Methods cannot declare type parameters.

Current limitations:

- Generic trait object is not supported;
- Multiple bounds such as `dyn Read + Close` are parsed for forward compatibility but rejected by the type checker;
- Explicit `as dyn Trait` is not supported;
- Pattern matching on `dyn Trait` is not supported.

## Properties and Derivations

User source code supports deriving `ToString`, `Eq` and `Hash` for structures and enumerations, including generic types:

```goml
#[derive(ToString, Eq, Hash)]
struct Key {
    name: string,
    version: int32,
}

#[derive(ToString, Eq, Hash)]
enum Entry[T, Marker] {
    Empty,
    Value(Vec[T]),
}
```

All fields or variant payloads must support the derived trait. For a generic definition, the generated impl constrains each distinct field or payload type that mentions a type parameter. In the example above the constraint is on `Vec[T]`, while the unused phantom parameter `Marker` receives no constraint. Compiler-owned runtime, intrinsic, and lang-item attributes remain unavailable to ordinary projects. User projects may use the `go_ffi` attribute described in the next section.

`ToString`, `Eq`, and `Hash` are prelude derive exports supplied by verified handlers in the toolchain's builtin sources. They are not hard-coded code generators in the compiler. Standard-library and third-party derives use the same handler and artifact mechanism, but they are not added to the prelude: import the trait or its package before using the derive name.

Derived `Eq` provides `Eq::eq(left, right)`, making the type usable with `==` / `!=` and satisfying the key constraints of `HashMap`.

### Programmable derive

A dependency package may export a compile-time derive handler:

```goml
package labels;

pub trait Label {
    fn label(self: Self) -> string;
}

#[comptime_derive(Label)]
pub fn derive_label(input: DeriveInput) -> DeriveOutput {
    let output = derive_output_new(input, "Label");
    let params = meta_param_list_new();
    meta_param_list_push(params, "self", derive_target_type(input));
    let body = meta_expr_string(derive_item_name(input));
    let method = meta_method("label", params, meta_type_call_site("string"), body);
    derive_output_add_method(output, method);
    output
}
```

Another package applies the handler through an imported package path:

```goml
use alice::labels as labels;
use labels::Label;

#[derive(Label)]
struct User {
    name: string,
}

#[derive(labels::Label)]
struct Group {
    name: string,
}
```

A public `#[comptime_derive(Name)]` handler exports the derive name `Name`, independently of its implementation function name. Export names are unique within one package and cannot contain `::`. The named form requires `pub`; private `#[comptime_derive]` functions remain implementation helpers. The unnamed public form remains available and exports the handler function's short name.

Derive names have their own namespace, but ordinary `use` declarations populate it alongside the type and trait namespaces. For example, after `use std::json;`, `use json::Serialize;` makes both the `Serialize` trait and a derive export named `Serialize` available as `Serialize`, so `#[derive(Serialize)]` works without another import form. An import alias applies to both namespaces: `use json::Serialize as JsonSerialize;` permits `#[derive(JsonSerialize)]`. A package import permits the qualified spelling `#[derive(json::Serialize)]`.

The standard JSON handler serializes a struct as an object with source-order fields. It uses externally tagged enums: a unit variant is a string, a tuple variant is an object whose value is an array, and a struct-like variant is an object whose value is another object. `Serialize::serialize` returns `json::Value`; `json::stringify` serializes and encodes a value in one step. Unit, booleans, strings, chars, numeric primitives, `Value`, `Vec[T]`, `Option[T]`, and two- or three-element tuples have standard implementations.

```goml
use std::json;
use json::Serialize;

#[derive(Serialize)]
struct User {
    name: string,
    active: bool,
}

fn encode_user(value: User) -> string {
    json::stringify(value)
}
```

A qualified custom derive must have the form `package_alias::export_name`, where `package_alias` is introduced by a `use` in the same source file. A bare library derive must be explicitly imported. Unimported canonical package paths and handlers visible only through a transitive dependency are rejected. If two explicit imports or a prelude derive and an explicit import provide the same bare name, the compiler reports ambiguity and requires a qualified name or import alias.

`#[comptime_derive]` and `#[comptime_derive(Name)]` are valid only on non-generic free functions. A public handler must have the exact signature `(DeriveInput) -> DeriveOutput`. Private functions with the unnamed attribute are compile-time-only helpers and are included in the interface when reachable from a public handler. A derive handler may call those helpers and ordinary `#[comptime]` functions, but it cannot be called from runtime code or ordinary value `comptime`. Derive handlers are not exported as runtime functions.

The first version resolves handlers from already compiled dependency interfaces. A handler cannot be defined and applied within the same package compilation. Put reusable handlers and their generated traits in a separate package. The target may be a generic struct or enum; the generated impl inherits its type parameters. `derive_output_add_predicate` and `derive_output_add_call_site_predicate` add the bounds required by generated methods.

The input reflection operations are:

```text
derive_item_name(input) -> string
derive_item_kind(input) -> int
derive_generic_count(input) -> int
derive_generic_name(input, index) -> string
derive_field_count/name/type(...)
derive_variant_count/name(...)
derive_variant_kind(input, variant) -> int
derive_variant_field_count/name/type(...)
derive_attribute_count/name/text(...)
derive_field_attribute_count/name/text(...)
derive_variant_attribute_count/name/text(...)
derive_variant_field_attribute_count/name/text(...)
derive_attribute(input, index) -> MetaAttribute
derive_field_attribute(input, field, index) -> MetaAttribute
derive_variant_attribute(input, variant, index) -> MetaAttribute
derive_variant_field_attribute(input, variant, field, index) -> MetaAttribute
derive_target_type(input) -> MetaType
derive_fresh_name(input, prefix) -> string
```

`derive_item_kind` returns zero for a struct and one for an enum. `derive_variant_kind` returns zero for a unit variant, one for a tuple variant, and two for a struct-like variant. The count operation for a nested attribute takes the owner indexes; its name and text operations take one additional attribute index. Field operations require a struct, while variant operations require an enum. Invalid kinds and indexes are compile-time errors at the `#[derive(...)]` site. Attributes may be attached to struct fields, enum variants, and tuple or named variant fields. `name` returns the attribute name and `text` returns its complete source spelling.

The structured attribute handle exposes `meta_attribute_name`, `meta_attribute_text`, `meta_attribute_has_argument_list`, `meta_attribute_argument_count`, `meta_attribute_argument_kind`, and `meta_attribute_argument_text`. Argument kind is `ident`, `path`, or `string`, and argument text is decoded rather than raw source spelling.

The structured output API provides opaque `MetaAttribute`, `MetaType`, `MetaExpr`, `MetaPattern`, `MetaArm`, `MetaBlock`, `MetaParamList`, `MetaMethod`, and list handles. Constructors use the `meta_type_*`, `meta_expr_*`, `meta_pattern_*`, `meta_arm*`, `meta_block_*`, and `meta_param_list_*` families. `meta_method` creates a concrete, non-generic trait method. A handler creates its single result with `derive_output_new` or `derive_output_new_call_site`, adds trait predicates and methods, and returns it.

The builder operations are:

```text
meta_type_named(input, name) -> MetaType
meta_type_call_site(name) -> MetaType
meta_type_list_new() -> MetaTypeList
meta_type_list_push(list, type) -> unit
meta_type_tuple(list) -> MetaType
meta_type_apply(type, arguments) -> MetaType
meta_type_array(element, length) -> MetaType
meta_type_kind/name(type) -> string
meta_type_argument_count/argument(type, index...) -> int | MetaType
meta_type_tuple_count/tuple_item(type, index...) -> int | MetaType
meta_type_array_length/array_element(type) -> int | MetaType
meta_type_function_parameter_count/parameter(type, index...) -> int | MetaType
meta_type_function_return(type) -> MetaType
meta_type_contains_generic(input, type) -> bool
meta_type_equal(left, right) -> bool

meta_expr_var(name) -> MetaExpr
meta_expr_unit/bool/int/string/char(value...) -> MetaExpr
meta_expr_integer(text, type) -> MetaExpr
meta_expr_field(value, name) -> MetaExpr
meta_expr_index(value, index) -> MetaExpr
meta_expr_unary(operator, value) -> MetaExpr
meta_expr_binary(operator, left, right) -> MetaExpr
meta_expr_call(input, name, arguments) -> MetaExpr
meta_expr_call_site(name, arguments) -> MetaExpr
meta_expr_trait_call(input, trait_name, method_name, arguments) -> MetaExpr
meta_expr_method_call(receiver, name, arguments) -> MetaExpr
meta_expr_constructor(name, arguments) -> MetaExpr
meta_expr_tuple/array(elements) -> MetaExpr
meta_expr_field_list_new() -> MetaExprFieldList
meta_expr_field_list_push(list, name, value) -> unit
meta_expr_struct(input, name, fields) -> MetaExpr
meta_expr_struct_call_site(name, fields) -> MetaExpr
meta_expr_if(condition, then, else) -> MetaExpr
meta_expr_match(value, arms) -> MetaExpr
meta_expr_cast(value, type) -> MetaExpr
meta_expr_return(value) -> MetaExpr
meta_expr_list_new() -> MetaExprList
meta_expr_list_push(list, expression) -> unit

meta_pattern_wild() -> MetaPattern
meta_pattern_bind(name) -> MetaPattern
meta_pattern_unit/bool/int/string/char(value...) -> MetaPattern
meta_pattern_tuple/array(patterns) -> MetaPattern
meta_pattern_constructor(name, patterns) -> MetaPattern
meta_pattern_field_list_new() -> MetaPatternFieldList
meta_pattern_field_list_push(list, name, pattern) -> unit
meta_pattern_struct(name, fields, has_rest) -> MetaPattern
meta_pattern_alias(name, pattern) -> MetaPattern
meta_pattern_or(patterns) -> MetaPattern
meta_pattern_range(start, end, inclusive) -> MetaPattern
meta_pattern_list_new() -> MetaPatternList
meta_pattern_list_push(list, pattern) -> unit
meta_arm(pattern, expression) -> MetaArm
meta_arm_guarded(pattern, guard, expression) -> MetaArm
meta_arm_list_new() -> MetaArmList
meta_arm_list_push(list, arm) -> unit

meta_block_new() -> MetaBlock
meta_block_let(block, name, value) -> unit
meta_block_let_mut(block, name, value) -> unit
meta_block_let_pattern(block, pattern, value) -> unit
meta_block_assign(block, target, value) -> unit
meta_block_expr(block, expression) -> unit
meta_block_finish(block, tail) -> MetaExpr
meta_block_finish_unit(block) -> MetaExpr
meta_param_list_new() -> MetaParamList
meta_param_list_push(list, name, type) -> unit
meta_method(name, parameters, return_type, body) -> MetaMethod

derive_output_new(input, trait_name) -> DeriveOutput
derive_output_new_call_site(input, trait_name) -> DeriveOutput
derive_output_add_predicate(output, type, trait_name) -> unit
derive_output_add_call_site_predicate(output, type, trait_name) -> unit
derive_output_add_method(output, method) -> unit
```

`meta_expr_unary` uses operator numbers `0..2` for `-`, `!`, and `~`. `meta_expr_binary` uses operator numbers `0..17` for `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`, `&&`, `||`, `<`, `>`, `<=`, `>=`, `==`, and `!=`, respectively. `meta_expr_integer` accepts a normalized integer literal string plus its exact integer type, so builders can represent values outside the host `int` range. `meta_expr_trait_call` resolves the trait in the handler's defining package and builds a static trait method call. `meta_type_equal` compares structural type identity. `meta_type_kind` returns `primitive`, `named`, `tuple`, `application`, `array`, `function`, or `dyn`; shape-specific accessors reject other kinds. List handles are mutable only through their matching `push` operation and remain local to one derive evaluation.

Unqualified names passed to `derive_output_new`, `derive_output_add_predicate`, `meta_type_named`, and `meta_expr_call` resolve in the handler's defining package. Their `_call_site` variants resolve in the target package. `derive_fresh_name` should be used for generated local bindings that must not collide with user names. Constructor patterns describe the target item and resolve at the call site.

The result is restricted to one trait `impl` for the annotated type. It cannot create types, traits, functions, constants, modules, imports, inherent impls, extern declarations, attributes, generic methods, associated types, or raw tokens. The generated impl is processed by ordinary name resolution, orphan and coherence checks, type checking, monomorphization, and backend lowering. Duplicate or invalid generated implementations are regular compiler diagnostics.

Handlers are deterministic and have no host access. Imported derive CTIR is verified as untrusted artifact data. Evaluation uses the ordinary compile-time limits plus a limit of 100,000 metadata and syntax-builder operations. Failures are anchored to the requesting derive attribute and include the compile-time derive call stack.

## Go FFI

The typed Go FFI binds a top-level GoML declaration to an exported package-level Go function:

```goml
#[go_ffi("strings", "ToUpper")]
extern fn to_upper(value: string) -> string;

#[go_ffi("strings", "Cut")]
extern fn cut(value: string, separator: string) -> (string, string, bool);

fn example() -> string {
    let (before, after, found) = cut("left:right", ":");
    if found { to_upper(before + after) } else { "" }
}
```

The first attribute argument is the Go import path and the second is an exported ASCII Go identifier. The GoML function name is local and may differ from the Go symbol. Add `pub` before `extern fn` to expose the binding through another GoML package; interface and Core artifacts preserve the Go import path and symbol.

The initial ABI supports values whose generated Go representations are already directly assignable:

| GoML type | Go representation |
| --- | --- |
| `bool`, numeric primitives, `string` | Corresponding Go primitive |
| `char` | `rune` / `int32` |
| `byte` | `byte` / `uint8` |
| `[T; N]` | `[N]T` |
| `Slice[T]` | `[]T` |
| `Channel[T]` | `chan T` |
| `unit` return | Go function with no result |
| `(A, B, ...)` return | Multiple Go results in the same order |

Tuple types are supported only as the complete return type. Tuple elements and array, slice, or channel elements must themselves be FFI-safe. Parameters cannot be `unit`.

The declaration must be monomorphic and must describe the Go function exactly. GoML does not inspect Go package type information during type checking; the Go compiler is the final authority for symbol existence and assignability. In particular, a Go named type such as `time.Duration` is not interchangeable with a GoML `int64` parameter even when its underlying representation is the same.

`Vec`, `Ref`, `HashMap`, `Option`, `Result`, user structs and enums, trait objects, function values, nested tuples, generic declarations, methods, callbacks, Go object handles, and automatic `error` conversion are not supported by this ABI. Write a small Go shim with an exported function and FFI-safe parameters when adapting such an API:

```go
package goshim

import "os"

func ReadText(name string) (string, bool) {
    data, err := os.ReadFile(name)
    return string(data), err == nil
}
```

```goml
#[go_ffi("example.com/myapp/goshim", "ReadText")]
extern fn read_text(name: string) -> (string, bool);
```

When the GoML module root contains `go.mod`, `goml build`, `goml run`, and test linking invoke Go in module mode, so local shim packages and declared Go module dependencies can be imported. Go workspace mode remains disabled. Without `go.mod`, builds retain the existing module-off behavior. Module-mode Go builds always execute and delegate dependency and source freshness to Go's own build cache, so changes to `go.mod`, `go.sum`, and local `.go` shims are observed.

This interface only calls Go from GoML. Exporting GoML functions to Go, calling methods, dynamic symbol lookup, and C ABI interoperation require separate mechanisms.

## test

### Test functions and properties

Top-level functions are marked as tests using `#[test]`:

```goml
package math;

use std::testing;

#[test]
fn addition_works() -> unit {
    testing::assert_eq(1 + 1, 2)
}
```

Test functions must meet the following rules:

- It can only be a top-level function, not an impl method, `extern fn`, structure, enumeration or trait;
- Cannot be named `main`;
- Cannot have parameters or type parameters;
- The return type must be `unit`;
- Canonical test IDs generated within the same test package must be unique.

Test functions do not require `pub`.`#[test]` does not accept parameters.When you need to skip a test by default, you can use `#[ignore]` without parameters or `#[ignore("reason")]` with a string reason; `#[ignore]` cannot be used alone without `#[test]`:

```goml
#[test]
#[ignore]
fn unfinished_case() -> unit {
    ()
}

#[test]
#[ignore("requires an external service")]
fn integration_case() -> unit {
    ()
}
```

`std::testing` provides the following assertions:

- `testing::fail(message)`: Fail the current test immediately;
- `testing::assert(condition)`: requires the condition to be `true`;
- `testing::assert_eq(actual, expected)`: requires two values ​​of the same type that implement `Eq + ToString` to be equal;
- `testing::assert_ne(actual, expected)`: Requires that two values ​​of the same type that implement `Eq + ToString` are not equal.

### White box testing and black box testing

The white-box test file and the source code under test are located in the same package directory. The file name must end with `_test.gom` and declare the same package name:

```text
math/
├── math.gom
└── math_test.gom
```

`math_test.gom`：

```goml
package math;

use std::testing;

#[test]
fn private_helper_works() -> unit {
    testing::assert_eq(private_helper(), 42)
}
```

The test build will merge the production source code and all `*_test.gom` in the same directory into a single package, so white-box testing can access the private top-level items of the package.These files will not participate in compilation when executing normal `goml check`, `goml build` or `goml run` for production targets.

Black-box tests are located in the `tests/` directory of the package under test.This directory as a whole constitutes a package named `tests`. The package under test should be imported explicitly and only its public API can be accessed; nested test suites cannot be created under `tests/`:

```text
math/
├── math.gom
└── tests/
    ├── api_test.gom
    └── smoke_test.gom
```

`math/tests/api_test.gom`：

```goml
package tests;

use alice::myapp::math;
use std::testing;

#[test]
fn public_add_works() -> unit {
    testing::assert_eq(math::add(20, 22), 42)
}
```

If the identity of the package under test is `alice::myapp::math`, the canonical identity of the test package in the above example is `alice::myapp::math::tests`.Ordinary package discovery will exclude the `tests` directory, and production packages cannot import black-box test packages.

### Check and run tests

`goml check`, `goml build` and `goml test` always process complete modules discovered from the current directory upwards, and do not accept package or file targets.`goml check` only checks the production source code by default; using `--tests` will also check all white-box and black-box tests after the production package check is successful:

```sh
goml check
goml check --tests
goml build
goml fmt
goml fmt --check
```

Test source code cannot be used to fix a production package that itself fails inspection.

`goml fmt` formats the current module's production files, internal tests, and black-box test packages using the same package graphs as builds and tests. It can be run from any nested package directory. Package discovery excludes `testdata`, build output, hidden directories, nested modules, and external dependencies. `goml fmt --check` only checks formatting and exits unsuccessfully when any source file would change.

Run the test using:

```sh
goml test [FILTER]
```

`FILTER` performs substring matching on the complete test display name, such as `goml test addition`.Common options include:

- `--kind internal|external|all`: run only white box, only black box or all tests, the default is `all`;
- `--list`: List matching tests without running them;
- `--ignored`: only run ignored tests;
- `--include-ignored`: Run normal tests and ignored tests at the same time;
- `--jobs N`: Number of parallel workers, default is `1`;
- `--timeout 500ms|30s|2m`: timeout for a single test, the default is `30s`;
- `--nocapture`: Let the standard output and standard error of the test directly inherit the terminal;
- `--format text|json`: Select text or line-by-line JSON results; JSON format cannot be used with `--nocapture` at the same time;
- `--target-dir`, `--dry-run` and `--compiler`: have the same meaning as project build commands.

Each test is executed in a separate runner process, and failure to exit and timeout do not affect other tests; `--jobs` controls the number of test processes running at the same time.Executing `goml test` requires an available Go toolchain to build the test runner.

### LSP and editor

LSP will construct the analysis package according to the production file, white box test file and black box test file respectively according to the path, so diagnosis, completion, hover and jump follow the corresponding visibility.`Run Test` CodeLens will appear on the `#[test]` function; the VS Code extension will save the dirty file first, and then call the module-level `goml test` with the complete test name and test type.

The custom `goml/expandedDerive` request returns the formatted AST after built-in and programmable derives have run for the requested document. It uses the same package aliases, explicit derive imports, ambiguity checks, dependency interfaces, CTIR verifier, and resource limits as `goml check`. The VS Code command `GoML: Show Expanded Derive` opens that result beside the source file.

`gomllsp` supports full-document formatting through `textDocument/formatting`. Formatting uses the latest unsaved document text and the fixed rules described in `docs/formatting.md`. Invalid documents are left unchanged.

## Built-in prelude

The following names can be used without `use`.

### `Option` and `Result`

```goml
enum Option[T] {
    None,
    Some(T),
}

enum Result[T, E] {
    Ok(T),
    Err(E),
}
```

Construction uses `Option::Some`, `Option::None`, `Result::Ok` and `Result::Err`.Patterns may use either those full names or contextual `Some`, `None`, `Ok` and `Err`.

`Option[T]` provides `is_some`, `is_none`, `unwrap_or`, and `unwrap_or_else`, plus type-changing generic methods:

- `value.map(map_fn) -> Option[U]`
- `value.and_then(next) -> Option[U]`
- `value.ok_or(error) -> Result[T, E]`

`Result[T, E]` provides `is_ok`, `is_err`, `unwrap_or`, and `unwrap_or_else`. Its type-changing generic methods are:

- `value.map(map_fn) -> Result[U, E]`
- `value.map_err(map_fn) -> Result[T, F]`
- `value.and_then(next) -> Result[U, E]`

### Output and string conversion

- `print[T: ToString](value: T) -> unit`
- `println[T: ToString](value: T) -> unit`
- `value.to_string() -> string`, suitable for values ​​that implement `ToString`
- `string.len() -> int` and `string.byte_len() -> int`, both returning the UTF-8 byte length
- `string.get(index: int) -> char`, decoding a character at a UTF-8 byte boundary
- `string.byte_get(index: int) -> uint8`
- `string.byte_slice(start: int, end: int) -> string`
- `string.is_char_boundary(index: int) -> bool`
- `string.decode_at(index: int) -> Option[(char, int)]`
- `string.to_bytes() -> Vec[uint8]`
- `string.chars() -> FnIterator[char]`
- `string.char_indices() -> FnIterator[(int, char)]`
- `string.starts_with(prefix: string) -> bool`
- `string.ends_with(suffix: string) -> bool`
- `string.contains(expected: string) -> bool`

The basic scalar types all implement `ToString`.String concatenation uses `+`.

The signatures of built-in key-related traits are `Eq::eq(self, other: Self) -> bool` and `Hash::hash(self) -> uint64`; user types can be handwritten impl, and structures and enumerations can also be generated using derive.

### `Ref[T]`

```goml
let cell = Ref::new(1);
let before = cell.get();
cell.set(before + 1);
```

API：

- `Ref::new(value) -> Ref[T]`
- `reference.get() -> T`
- `reference.set(value) -> unit`
- `ptr_eq(a, b) -> bool`, compare reference identities

The built-in `Eq` and `Hash` implementations for `Ref[T]` use reference identity and do not require `T` to implement `Eq` or `Hash`. Mutating the referenced value therefore does not change equality or hashing.

### fixed array

```goml
let mut values: [int32; 3] = [1, 2, 3];
let first = values[0];
values[1] = 20;
```

The index type is `int`.The underlying `array_get` and `array_set` can also be called; index syntax is preferred for daily code.

### `Vec[T]`

```goml
let values = Vec::[1, 2, 3];
let first = values.get(0);
```

`Vec::[...]` creates a vector literal. Items are evaluated once from left to right. A trailing comma is allowed. An empty literal needs an expected element type, for example `let values: Vec[int32] = Vec::[];`.

Commonly used methods:

- `Vec::new() -> Vec[T]`
- `Vec::with_capacity(capacity) -> Vec[T]`
- `push(value) -> unit`
- `pushed(value) -> Vec[T]`
- `get(index) -> T`
- `set(index, value) -> unit`
- `len() -> int`
- `capacity() -> int`
- `is_empty() -> bool`
- `reserve(additional) -> unit`
- `truncate(len) -> unit`
- `clear() -> unit`
- `last() -> Option[T]`
- `pop() -> Option[T]`
- `swap(left, right) -> unit`
- `swap_remove(index) -> T`
- `insert(index, value) -> unit`
- `remove(index) -> T`
- `reverse() -> unit`
- `extend(other) -> unit`
- `slice(start, end) -> Slice[T]`
- `iter() -> FnIterator[T]`

`vec[index]` is equivalent to reading the element, `vec[index] = value;` modifies the element.

### `Slice[T]`

`Slice[T]` is a bounded read-only view on the `Vec[T]` storage:

```goml
let view: Slice[int32] = values.slice(1, 3);
let item = view.get(0);
```

Commonly used methods: `get`, `len`, `sub`, `iter`.`view[index]` can be written, but `view[index] = value;` cannot be written.

### `HashMap[K, V]`

The key type must implement both `Hash` and `Eq`:

```goml
let counts = HashMap::{
    "a" => 1,
    "b" => 2,
};
let value: Option[int32] = counts.get("a");
```

`HashMap::{ key => value, ... }` evaluates each key followed by its value, proceeding from left to right. A trailing comma is allowed. Later duplicate keys overwrite earlier entries. An empty literal needs an expected key and value type, for example `let counts: HashMap[string, int32] = HashMap::{};`. The `=>` spelling reuses the same token used by match arms.

Commonly used methods: `new`, `get`, `set`, `remove`, `len`, `contains`, and `entries`. `entries()` returns a snapshot `Vec[(K, V)]`.

Index reading returns `Option[V]`, and index assignment writes `V`:

```goml
let value = counts["a"];
counts["b"] = 2;
```

### `Channel[T]`

`Channel[T]` is a Go channel backend that supports buffered and unbuffered communication:

```goml
let channel = Channel::[string]::new(0);
go || channel.send("ready");
let value: Option[string] = channel.recv();
channel.close();
```

Commonly used methods:

- `Channel::[T]::new(capacity: int) -> Channel[T]`
- `send(value: T) -> unit`
- `recv() -> Option[T]`, returns `Option::None` when closed and drained
- `close() -> unit`

Capacity `0` creates an unbuffered channel.Sending to an unbuffered channel should generally be concurrently received by another `go` closure, and vice versa.

### Iterator

The built-in protocols are:

```goml
trait Iterator {
    type Item;
    fn next(self) -> Option[Self::Item];
}

trait IntoIterator {
    type Item;
    type IntoIter: Iterator;
    fn into_iter(self) -> Self::IntoIter;
}
```

The protocol entry points are `FnIterator::from_fn(next_fn)`, `iterator.next()` or `Iterator::next(iterator)`. `range(start, end)` and `start..end` produce incrementing half-open `FnIterator[int]` values; `start >= end` is empty.

New code should import `std::iter` for iterator construction, adapters, and consumers:

```goml
use std::iter;

let values = iter::map(Vec::[1, 2, 3].iter(), |value: int| value * 2);
let total = iter::fold(values, 0, |sum: int, value: int| sum + value);
```

- producers: `empty`, `once`, and `from_fn`
- adapters: `map`, `filter`, `filter_map`, `take`, `take_while`, `skip`, `skip_while`, `enumerate`, `zip`, `chain`, `inspect`, and `map_while`
- consumers: `fold`, `collect`, `find`, `find_map`, `any`, `all`, `count`, `position`, `nth`, `last`, `for_each`, and `reduce`

Iterators are single pass. Fixed arrays use native indexed `for` lowering. `Vec[T]` and `Slice[T]` can be directly used in `for`, and a value implementing `Iterator` is also directly iterable through the identity `IntoIterator`.

## Standard library package

The standard library is not equal to prelude and needs to be imported by package:

```goml
use std::ascii;
use std::bytes;
use std::cmp;
use std::collections;
use std::env;
use std::fs;
use std::io;
use std::iter;
use std::json;
use std::num;
use std::path;
use std::process;
use std::task;
use std::testing;
use std::text;
use std::time;
```

Current public entrances include:

- `ascii::is_ascii`, character-class predicates, ASCII case conversion and comparison, and `escape_default`
- `bytes::Bytes`, a mutable byte buffer with conversion to and from strings and `Vec[uint8]`
- `cmp::Ordering`, `Ord`, `Reverse`, comparison helpers, and two-value minimum, maximum, and clamping operations
- `collections::Arena`, `BitSet`, `Deque`, `HashSet`, `IndexMap`, `IndexVec`, `Interner`, and `Stack`
- `collections::sort`, `stable_sort`, `binary_search`, `min`, `max`, comparator-based variants, `position_by`, `contains`, `dedup_by`, and `dedup`
- `env::args`, `current_dir`, `current_exe`, and `var`
- `fs::read_file`, `write_file`, byte I/O, directory operations, path inspection, and `sha256_file`
- `io::print`, `println`, `eprint`, `eprintln`, and byte-oriented standard stream I/O
- `iter::empty`, `once`, `from_fn`, iterator adapters, and single-pass consumers
- `json::Value`, `parse`, `encode`, `Serialize`, `stringify`, `field`, and typed `as_*` accessors
- `num::parse_int`, `parse_int_radix`, `parse_uint`, `parse_uint_radix`, `parse_float32`, and `parse_float64`
- `path::join`, `clean`, `is_absolute`, component inspection, and `absolute`
- `process::Command`, `ExitStatus`, `Output`, `exit`, and `look_path`
- `task::Scope`, `Task[T]`, `CancelToken`, `WaitResult[T]`, `scope`, and `try_scope`
- `testing::fail`, `assert`, `assert_eq`, and `assert_ne`
- `text::StringBuilder`, `find`, `rfind`, `starts_with_at`, `trim`, `trim_start`, `trim_end`, `split`, `split_once`, `lines`, `replace`, `join`, `repeat`, `is_ascii`, `eq_ignore_ascii_case`, `to_ascii_lowercase`, and `to_ascii_uppercase`
- `time::Duration`, `Instant`, `SystemTime`, `sleep`, and `sleep_with`

`std::ascii` operates on `byte`. Classification and case conversion use only the 7-bit ASCII range, and bytes above `0x7f` remain unchanged. `escape_default` emits short escapes for tabs, carriage returns, newlines, quotes, and backslashes, preserves printable ASCII, and uses lowercase `\\xNN` escapes for other bytes.

`std::cmp` provides total ordering for `unit`, `bool`, `string`, `char`, and all signed and unsigned integer types. Floating-point values intentionally do not implement `cmp::Ord` because NaN does not form a total order. `cmp::compare` returns `Ordering::Less`, `Equal`, or `Greater`; `Ordering` supports predicates, reversal, lexicographic chaining, and conversion to the negative/zero/positive integer convention. `cmp::Reverse[T]` reverses an existing total ordering. `cmp::clamp` returns an error when the minimum exceeds the maximum.

### Structured concurrency

`std::task` creates lexical task scopes on top of goroutines:

```goml
use std::task;

fn load_pair() -> Result[(int, int), string] {
    task::try_scope(
        |scope: task::Scope| {
            let left = scope.spawn_try(|cancel| load_left(cancel));
            let right = scope.spawn_try(|cancel| load_right(cancel));
            Result::Ok((left.join()?, right.join()?))
        },
    )
}
```

The contextual `scope` and `spawn` forms provide the same structured lifetime with a hidden scope capability:

```goml
use std::task;

fn load_pair() -> Result[(int, int), string] {
    scope {
        let left = spawn |cancel| load_left(cancel);
        let right = spawn |cancel| load_right(cancel);
        Result::Ok((left.join()?, right.join()?))
    }
}
```

`scope { body }` is lowered before HIR checking to `task::scope(|hidden_scope| body)`, and each directly nested `spawn |cancel| body` is lowered to `hidden_scope.spawn(|cancel| body)`. The file must import `std::task`. Directly nested lexical scopes use `scope_with` and inherit their parent's cancellation. The hidden scope value cannot be named or returned. A lexical `spawn` cannot cross a user closure boundary, which prevents a returned closure from capturing the capability; create a nested `scope` inside that closure instead. `scope` and `spawn` remain ordinary identifiers outside these contextual forms.

`scope` stops accepting new work after its body returns, waits for every direct child task, and then returns the body's value. It still waits when a `Task[T]` handle is discarded. `Task::join` may be called repeatedly or concurrently; every call observes the same stored result. `Scope::try_spawn` returns `Some(task)` when it registers the task before closing begins and `None` after the scope starts closing. Ordinary `Scope::spawn` retains the stricter runtime-error behavior for closed scopes.

`try_scope` cancels its scope when the body returns `Result::Err`, waits for all direct children to exit, and then returns the original error. `Scope::spawn_try` also cancels sibling tasks as soon as its child returns `Result::Err`. A nested scope inherits cancellation when it is created with `scope_with(parent_token, body)` or `try_scope_with(parent_token, body)`.

Cancellation is cooperative. `Scope::cancel` changes the state observed by `CancelToken::is_cancelled`; it does not forcibly terminate a goroutine. Blocking work can use:

- `task::recv_with(token, channel) -> WaitResult[Option[T]]`
- `task::send_with(token, channel, value) -> WaitResult[unit]`
- `time::sleep_with(token, duration) -> WaitResult[unit]`
- `Command::output_with(token) -> Result[WaitResult[Output], string]`
- `Command::status_with(token) -> Result[WaitResult[ExitStatus], string]`

`WaitResult::Cancelled` means cancellation woke the operation. Process cancellation uses the host command context, so the scope waits for the process operation to return before it exits. Task scopes never close user channels automatically. `active_scope_count()` exposes the number of live runtime scopes for tests and leak diagnostics.

GoML has no lifetime or linear type system, so a `Scope` value can currently escape its body. Calling `spawn` after the scope begins closing is a runtime error. Panic remains a fatal runtime exception and is not converted into `Result`. A panic in the scope body or a child task cancels sibling tasks, waits for them, removes the runtime scope, and is then re-raised in the scope owner.

`join_all` returns values in input order. `join_all_results` waits for every task and returns errors in input order, independent of goroutine scheduling.

`join_all_indexed_results` preserves each error's input index. `for_each_concurrent(limit, values, body)` runs at most `limit` calls at once, uses one worker when the limit is non-positive, waits for every value, and returns indexed errors in input order. `ConcurrencyLimit::run` can apply the same cooperative limit to custom task layouts.

`race(bodies)` returns the first completed value, cancels the remaining bodies, and still waits for every losing body to exit. It returns `None` for an empty input. A body that does not cooperate with cancellation can therefore delay the return from `race`.

### `collections::IndexMap[K, V]`

`IndexMap` is an insertion-ordered hash map. Its key type must implement `Eq` and `Hash`:

```goml
use std::collections;

let headers: collections::IndexMap[string, string] =
    collections::IndexMap::new();
headers.insert("content-type", "text/plain");
headers.insert("content-length", "12");
headers.insert("content-type", "application/json");

for (name, value) in headers {
    println(name + ": " + value)
}
```

Inserting a new key appends it to the iteration order. Replacing an existing value does not move the key. Removing and inserting the key again appends it to the end.

Common methods are `new`, `with_capacity`, `len`, `is_empty`, `contains`, `get`, `insert`, `remove`, `reserve`, `clear`, `entries`, `keys`, `values`, and `iter`. `insert` and `remove` return the previous value as `Option[V]`. `entries`, `keys`, and `values` return ordered snapshots, while `iter` and `for` traverse `(K, V)` pairs in insertion order.

The implementation uses a sparse open-addressed index table and an insertion-ordered entry array. Deleted entries become tombstones and are compacted during later growth or when deletion density becomes high. Lookup, insertion, and removal are expected O(1); iteration and compaction are O(n). Structural mutation while an iterator is active is unsupported.

`IndexMap` does not currently have literal or indexing syntax. Use `insert` and `get`.

File system operations use `Result[..., string]` to report errors, and can be combined with `?`.

Text search indices are UTF-8 byte offsets, matching the indices accepted by the built-in string APIs. `starts_with_at` returns false for out-of-range and non-character-boundary offsets. `rfind` returns the last matching byte offset. Trimming recognizes ASCII whitespace. Splitting on an empty separator returns the original string as one item, while `split_once` with an empty separator returns `Option::None`.

Numeric parsing returns `Result[_, string]`. Integer radix parsing accepts the host parser's supported radices and reports invalid radices, malformed input, and overflow as `Result::Err`.

Sorting mutates a `Vec[T]` in place. `sort` and `stable_sort` use `cmp::Ord`; `sort_by_ordering` and `stable_sort_by_ordering` use `cmp::Ordering`. The older `sort_by` and `stable_sort_by` variants accept negative/zero/positive integer comparators. All current sorting variants are stable. `binary_search` and its comparator-based variants expect the vector to already be ordered and return the first matching index.

`Duration` stores a non-negative number of nanoseconds and offers constructors and whole-unit accessors for nanoseconds, microseconds, milliseconds, and seconds. Subtraction saturates at zero. `Instant` is monotonic and is suitable for elapsed-time measurement. `SystemTime` exposes Unix nanosecond, millisecond, and second timestamps. `time::sleep` blocks the current goroutine for a `Duration`.

## Comparison of common writing errors

| Don't generate | GoML writing method |
| --- | --- |
| `Vec<int>` | `Vec[int]` |
| `fn id<T>(x: T) -> T` | `fn id[T](x: T) -> T` |
| `id::<int32>(1)` | `id::[int32](1)` |
| Ordinary function `id[int32](1)` | `id::[int32](1)`, or rely on parameter/result type inference |
| `1i32`、`1u64`、`1.0f32` | Use the expected type, such as `let value: uint64 = 1;` |
| Non-ASCII source text in `b"é"` | Use a `string`, or write its encoded bytes explicitly such as `b"\xC3\xA9"` |
| A bare block as a control-flow header value | Parenthesize it, for example `if ({ prepare(); ready() }) { ... }` |
| `let mut x: &T` | Use value `T` or `Ref[T]` as required |
| Write `if cond { value }` in the value position | `if cond { value } else { other }` |
| `let Option::Some(x) = value;` | `if let Option::Some(x) = value { ... };` or `match` |
| `let Point { x } = point;` | `let Point { x, .. } = point;` |
| Endless `match` | Complete variant or `_` branch |
| `x++`、`x--` | `x += 1;`、`x -= 1;` |
| Assign through an immutable structure binding | Declare the binding with `let mut`, or create a new value with `Point { field: value, ..point }` |
| `var x = 1`、`x := 1` | `let x = 1;` |
| A loop with a condition | `while condition { ... }` |
| `for i := 0; ...` | `while`, or `for i in start..end` |
| `switch` | `match` |
| `null`、`nil` | `Option::None` |
| `throw`, exception | `Result` and `?` |
| `float_value as int32` | Floating point to integer conversion is not supported; use dedicated parsing or conversion APIs |
| `dyn A + B` | Use one dyn-safe trait; multiple bounds are reserved syntax but not yet supported |
| `dyn TraitWithAssociatedType` | Bind every associated type, for example `dyn Iterator[Item = int]` |
| `use pkg::*` | List the required public items explicitly with `use pkg::{A, B};` |
| `mod`、`crate::`、`super::` | Directory packages and canonical `use` paths |
| `fn helper` inside function | Top-level function or local closure |
| Unannotated user `extern fn` | Use a normal GoML function or `#[go_ffi("import/path", "ExportedSymbol")] extern fn` |
| Call an ordinary function from `comptime` | Mark a supported free function with `#[comptime]` |
| Capture a runtime local in `comptime` | Pass a literal or compile-time value to a `#[comptime]` function |

## Informal Grammar Quick Facts

The following EBNF only describes the canonical form that should be generated; `?` means optional, `*` means repeated, and the terminator is placed in quotes.

```text
file          = package_decl? use_decl* item*
package_decl  = "package" lower_ident ";"
use_decl      = "pub"? "use" path ("as" ident | "::" "{" use_items "}")? ";"
use_items     = use_item ("," use_item)* ","?
use_item      = ident ("as" ident)?
path          = ident ("::" ident)*

item          = attribute* visibility? function
              | attribute* visibility? constant
              | attribute* visibility? struct_def
              | attribute* visibility? enum_def
              | attribute* visibility? trait_def
              | attribute* impl_def
              | go_ffi_extern
visibility    = "pub"
attribute     = "#[" attribute_body "]"
comptime_attribute = "#[" "comptime" "]"
comptime_derive_attribute = "#[" "comptime_derive" ("(" ident ")")? "]"
derive_attribute = "#[" "derive" "(" path ("," path)* ")" "]"
go_ffi_attribute = "#[" "go_ffi" "(" string_literal "," string_literal ")" "]"
go_ffi_extern = go_ffi_attribute visibility? "extern" "fn" lower_ident
                param_list return_type? ";"

function      = "fn" lower_ident generic_params? param_list return_type? where_clause? block
constant      = "const" lower_ident ":" type "=" expression ";"
method        = visibility? "fn" lower_ident generic_params? param_list return_type? where_clause? block
generic_params = "[" generic_param ("," generic_param)* "]"
generic_param = upper_ident (":" trait_set)?
param_list    = "(" (parameter ("," parameter)*)? ")"
parameter     = lower_ident ":" type | "self"
return_type   = "->" type

struct_def    = "struct" upper_ident type_names? "{" struct_fields? "}"
struct_fields = struct_field ("," struct_field)* ","?
struct_field  = visibility? lower_ident ":" type
enum_def      = "enum" upper_ident type_names? "{" variants? "}"
variants      = variant ("," variant)* ","?
variant       = upper_ident
              | upper_ident "(" type_list? ")"
              | upper_ident "{" variant_fields? "}"
variant_fields = lower_ident ":" type ("," lower_ident ":" type)* ","?
type_names    = "[" upper_ident ("," upper_ident)* "]"

trait_def     = "trait" upper_ident generic_params? (":" trait_set)? where_clause?
                "{" trait_member* "}"
trait_member  = "type" upper_ident (":" trait_set)? ";"
              | "fn" lower_ident generic_params? param_list return_type? where_clause?
                (";" | block)

impl_def      = "impl" generic_params? trait_ref "for" type where_clause?
                "{" impl_member* "}"
              | "impl" generic_params? type where_clause?
                "{" method* "}"
impl_member   = "type" upper_ident "=" type ";" | method

trait_set     = trait_ref ("+" trait_ref)*
trait_ref     = path type_args?
where_clause  = "where" where_predicate ("," where_predicate)* ","?
where_predicate = type ":" trait_set | type "=" type

type          = primitive_type
              | path type_args?
              | "dyn" dyn_bound ("+" dyn_bound)*
              | "[" type ";" integer_literal "]"
              | "(" type_list ")"
              | "()" "->" type
              | type "->" type
type_args     = "[" type_list "]"
type_list     = type ("," type)* ","?
dyn_bound     = path dyn_args?
dyn_args      = "[" (type ",")* dyn_assoc ("," dyn_assoc)* ","? "]"
              | "[" type_list "]"
dyn_assoc     = upper_ident "=" type

block         = "{" statement* expression? "}"
statement     = "let" "mut"? pattern (":" type)? "=" expression ("else" block)? ";"
              | "defer" expression ";"
              | assign_target assignment_operator expression ";"
              | expression ";"
              | control_expression

assignment_operator = "=" | "+=" | "-=" | "*=" | "/=" | "%="
                    | "&=" | "|=" | "^=" | "<<=" | ">>="

expression    = literal | raw_string | byte_string | raw_byte_string
              | interpolated_string | path | tuple | array | block
              | vec_literal | hashmap_literal | struct_literal | closure
              | call | field | index | unary | binary | cast | range_expression
              | try_expression
              | comptime_expression
              | if_expression | match_expression | while_expression | loop_expression | for_expression
              | scope_expression | spawn_expression
              | "return" expression?
              | "break" loop_label? expression?
              | "continue" loop_label?
              | "go" expression

interpolated_string = "f\"" (string_text | "{{" | "}}" | "{" expression "}")* "\""
byte_string   = "b\"" byte_string_content* "\""
raw_byte_string = "br\"...\"" | "br#\"...\"#" | "br##\"...\"##" | ...
raw_string    = "r" raw_hashes? "\"" raw_text "\"" raw_hashes?
vec_literal   = "Vec" "::" "[" (expression ("," expression)* ","?)? "]"
hashmap_literal = "HashMap" "::" "{"
                  (expression "=>" expression
                   ("," expression "=>" expression)* ","?)?
                  "}"
struct_literal = path "{" (struct_literal_field ("," struct_literal_field)*
                 ("," ".." expression)? ","? | ".." expression ","?)? "}"
struct_literal_field = lower_ident (":" expression)?

control_expression = if_expression | match_expression | while_expression | loop_expression | for_expression
if_expression = "if" expression block ("else" (block | if_expression))?
              | "if" "let" pattern "=" expression block
                ("else" (block | if_expression))?
match_expression = "match" expression
                   "{" (match_arm ",")* match_arm? "}"
match_arm     = pattern ("if" expression)? "=>" (expression | block)
while_expression = loop_label_decl? "while" expression block
              | loop_label_decl? "while" "let" pattern "=" expression block
loop_expression = loop_label_decl? "loop" block
for_expression = loop_label_decl? "for" pattern "in" expression block
loop_label_decl = loop_label ":"
loop_label    = "'" lower_ident
scope_expression = "scope" block
spawn_expression = "spawn" closure
comptime_expression = "comptime" block
closure       = "||" (expression | block)
              | "|" closure_params? "|" (expression | block)
cast          = expression "as" integer_type
range_expression = expression (".." | "..=") expression

pattern       = or_pattern
or_pattern    = alias_pattern ("|" alias_pattern)*
alias_pattern = ident "@" alias_pattern | range_pattern
range_pattern = primary_pattern ((".." | "..=") range_endpoint)?
range_endpoint = "-"? integer_literal | char_literal
primary_pattern = ident | "mut" lower_ident | "_" | literal | "-" numeric_literal | "()"
              | "(" pattern ")"
              | "(" pattern "," (pattern ("," pattern)* ","?)? ")"
              | path
              | path "(" pattern_list? ")"
              | path "{" struct_pattern_fields? "}"
              | array_pattern
pattern_list  = pattern ("," pattern)* ","?
struct_pattern_fields = struct_pattern_field ("," struct_pattern_field)*
                        ("," "..")? ","?
                      | ".." ","?
struct_pattern_field = ident (":" pattern)?
array_pattern = "[" (array_pattern_item ("," array_pattern_item)* ","?)? "]"
array_pattern_item = pattern | ".." | ident "@" ".."
```

The parser will do some error recovery for commas and semicolons, but the code agent should always generate the above canonical form: list items separated by commas, `let`, assignments and ordinary non-tail expressions with semicolons, control flow statements without semicolons, trait method signatures with semicolons.The sequence pattern contains at most one rest; the `..` in the structure pattern appears at most once and must be at the end.

## Verify generated code

After building stage 1, verify a standalone source file from the repository root:

```sh
stage1/bin/gomlc run-single path/to/main.gom
```

For a project, run the installed driver from anywhere inside its module:

```sh
goml check
goml build
goml check --tests
goml test
goml run
goml fmt
```

`goml check`, `goml build`, `goml test`, and `goml fmt` always operate on the complete module and do not accept package or file targets. `goml run [TARGET]` accepts an optional entry package file or directory when a module has multiple executable packages.

When you need to inspect a compilation phase, add `--dump-ast`, `--dump-expanded-ast`, `--dump-hir`, `--dump-tast`, `--dump-ctir`, `--dump-core`, `--dump-mono`, `--dump-lift`, `--dump-anf`, or `--dump-go` to `gomlc run-single`. `--dump-ast` shows source lowering before derive expansion, while `--dump-expanded-ast` includes every generated implementation.

The code agent should at least run the corresponding `goml check` or `gomlc run-single` before submitting the source code; when modifying the test, it should also run `goml check --tests` and the related `goml test`.When type inference fails, give priority to adding local result types, empty container types, closure parameter types, or using UFCS instead of rewriting to unsupported Rust/Go syntax.
