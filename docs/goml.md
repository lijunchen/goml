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
8. `let` and assignment statements must end with a semicolon.Local binding must be declared with `let mut` before reassignment; the semicolon can be omitted for `if`, `match`, `while` and `for` as statements.
9. Enumeration constructors and patterns prefer to write their full names, such as `Option::Some(value)`, to avoid name conflicts.
10. For cross-package calls, write `alias::item`.Top-level items, structure fields, and native methods must all be marked with `pub` as required.
11. Before using trait method syntax across packages, import the package first and then write `use alias::Trait;`; when in doubt, use UFCS: `Trait::method(value)`.
12. Do not generate `mod`, `crate::`, `self::`, `super::`, root paths `::x`, Rust references, Go `var` / `:=` or user `extern fn`.
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

- Package names, `use ... as` aliases, functions, methods, parameters, local bindings and fields must start with a lowercase letter or `_`;
- Structures, enumerations, traits, enumeration variants, generic parameters, and associated types must start with a capital letter;
- Paths retain the appropriate case for the referenced name.

Enumeration variants should be used via `Enum::Variant` to avoid name conflicts.

Common keywords include:

```text
package use as pub fn struct enum trait impl for type where
let mut if else match while for in break continue return go dyn
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
| integer | `0`、`42`、`1_000` | Determined by context; defaults to `int` when unconstrained |
| floating point number | `1.25`、`1e3`、`2.5e-2` | Determined by context; defaults to `float64` when unconstrained |
| string | `"text"` | Type is `string` |
| character | `'a'`、`'\n'`、`'\u0041'` | Type is `char`, representing a Unicode scalar value |

Numbers only support decimal form, no type suffix.The `_` delimiter can be used between two numbers; floating point numbers support `e`/`E` exponent and optional exponent sign.When using a decimal point, there must be digits on both sides of the decimal point.There are currently no hexadecimal or binary literals.Negative numbers are composed of unary `-` and positive numeric literals.

Unsuffixed numbers can get the width from the context:

```goml
let small: uint8 = 42;
let ratio: float32 = 0.5;
let values: [int16; 3] = [1, 2, 3];
```

Strings support `\"`, `\\`, `\n`, `\r`, `\t`, `\b`, `\f`, `\/` and four-digit `\uXXXX` escaping; characters are escaped using the same set of control characters, and `\'` is used to represent single quotes. Ordinary strings cannot span lines.

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

There are currently no globs, braced imports, or plain single imports.The following special form only adds traits from a loaded package to the method calling scope:

```goml
use alice::rendering::api;
use api::Render;
```

Afterwards, the specific value can be written as `value.render()`.Even if the trait is not added to the method scope, you can still write a qualified call to `api::Render::render(value)`.

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
| floating point | `float32`、`float64` | IEEE floating point |
| string | `string` | Go string backend |
| character | `char` | Compile to Go `rune` |
| tuple | `(int32, string)` | Tuples in type syntax have at least two elements |
| fixed array | `[int32; 4]` | The length is part of the type |
| function | `(int32, string) -> bool` | parameter type list to return type |
| Generic application | `Option[int32]`、`pkg::Box[string]` | Use square brackets |
| channel | `Channel[int]` | Go channel backend |
| trait object | `dyn Render` | A single, non-generic, unassociated type dyn-safe trait |
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

### Type syntax not currently available

GoML has no Rust references and lifetimes, raw pointers, nullable types, slice literals, type aliases, or union types.Use `Ref[T]` to express shared variable units, use `Option[T]` to express optional values, and use `Slice[T]` to express read-only continuous views.

`A + B` is only allowed to appear in trait bound or supertrait lists, and cannot be written as ordinary parameters/return types, nor can it be written as `dyn A + B`.

`Self` is only used in the trait signature and the type position of impl; ordinary top-level functions cannot use `Self` as an implicit type parameter.

## top level definition

The top level of an ordinary source code file should only contain:

- `fn`
- `struct`
- `enum`
- `trait`
- `impl`

`package` and `use` can only appear at the beginning of a file.The top level cannot write local variables or arbitrary execution statements.

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

Currently, Rust's `..old` structure update syntax is not supported, nor is direct field assignment `point.x = value;` supported.When you need to update, you can rebuild the entire value, or put the modified content that needs to be shared into `Ref[T]`.

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

Naked variant names are available if they can be resolved uniquely, but multiple enumerations may define variants with the same name.Variant names must be uppercase; `Enum::Variant` is preferred when generating code.

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
fn collect_items[T, I: Iterator](iterator: I) -> Vec[T]
where
    I::Item = T,
{
    iterator_collect(iterator)
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

Don't write `identity[int32](1)` or Rust's `identity::<int32>(1)`.Generic trait UFCS and associations with generic types also put `::[...]` before the member name, such as `Convert::[int32]::convert(value)` and `Channel::[string]::new(0)`.

Only top-level function declarations should introduce the function's own type parameters.Local named function does not exist; use closure.Closures do not have generics, and methods in impl should not declare their own type parameters.Structures, enumerations, traits, and impl themselves can have type parameters.

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

`let`, ordinary assignments, and general non-tail expression statements require semicolons.When used as statements and followed by code, `if`, `match`, `while` and `for` can omit the semicolon; other expression statements still require semicolons.Functions, structures, enumerations, traits, impl and blocks themselves are not declared with a semicolon after them.

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

You cannot do `let` with enumeration or literal patterns that may fail; use `if let` or `match` instead.

### `mut` and assignment

```goml
let mut count = 0;
count = count + 1;
```

Ordinary local bindings without `mut` cannot be reassigned.Currently ordinary assignment targets are mutable local variables or supported index locations; compound assignments, increment/decrement, or direct field assignments are not supported:

```text
x += 1
x++
point.x = 1
```

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
- Function call `f(a, b)`
- Field access `value.field`
- Tuple projection `pair.0`, `triple.2`
- Method call `value.method(arg)`
- Index `value[index]`
- Unary and binary operations
- Integer conversion `value as uint32`
- Half-open range expression `start..end`
- `if`、`if let`、`match`、`while`、`while let`、`for`
- closure
- `return`, `break`, `continue`, `go` and `?`

There is currently no separate arbitrary block expression syntax; blocks appear within functions, closures, and control flow constructs.

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

`start..end` constructs an incrementing half-open `FnIterator[int]`, which can be used directly in `for`:

```goml
for value in 0..10 {
    println(value)
}
```

Both ends are `int`; the range is empty when `start >= end`.Range expressions cannot be chained, and there is currently no `..=` in the expression position.The `..` and `..=` in the pattern are another set of range pattern syntax.

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
        Option::Some(inner) => inner,
        Option::None => fallback,
    }
}
```

The matched expression is evaluated only once, and the pattern is tried from top to bottom.All branches must produce compatible types, and the compiler is required to cover all possible values.

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

The condition must be `bool` and the loop result is `unit`.There is currently no `break` with a value.

`while let` evaluates the right-hand expression once in each round; when the match is successful, it enters the loop body and provides pattern binding, and when the match fails, it exits the loop:

```goml
while let Option::Some(value) = iterator.next() {
    println(value);
};
```

The loop body must return `unit`.Pattern binding is only visible within the loop body.

### `for`

```goml
let values: Vec[int32] = Vec::new();
values.push(10);
values.push(20);

for value in values {
    println(value);
};
```

`for pattern in source { ... }` accepts values ​​that implement `IntoIterator`.Both the source expression and the `into_iter` transformation are executed only once.The pattern must be irrefutable and the loop body must return `unit`.`start..end` can be used directly as a native `int` range.

Tuple destructuring can be used directly in loops:

```goml
for (key, value) in pairs {
    println(key + value.to_string());
};
```

`Vec[T]`, `Slice[T]` and all `Iterator` have corresponding `IntoIterator` implementations.

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

`break` and `continue` can only appear in loops, and neither has a value.They are divergent control expressions that can appear in `if`, `match`, or other value positions.

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
| Enumerate unloaded variants | `Color::Red` |
| Enum load variants | `Option::Some(value)` |
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

The number of constructor parameters of the enumeration mode must be consistent with the definition.Naked variant names are available if they can be resolved uniquely in the entire current package, including variants defined in other files in the same package; ambiguities are reported when there are multiple candidates.In order to make the source code stable and clear, `Enum::Variant` should still be written first.

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

There are currently no `ref` or `mut` subpatterns, and pattern matching on `dyn Trait` is not supported.

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

Currently, the trait method has no default implementation, and the method cannot declare its own type parameters.

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
}
```

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

Current dyn-safe conditions:

- Traits cannot have type parameters;
- Traits cannot declare associated types;
- Each method must have a first receiver parameter of exactly type `Self`;
- `Self` cannot appear in other parameters and return types.

Current limitations:

- Generic trait object is not supported;
- `dyn A + B` is not supported;
- Explicit `as dyn Trait` is not supported;
- Pattern matching on `dyn Trait` is not supported.

## Properties and Derivations

User source code currently supports deriving `ToString`, `Eq` and `Hash` for non-generic structures and enumerations:

```goml
#[derive(ToString, Eq, Hash)]
struct Key {
    name: string,
    version: int32,
}
```

All fields or variant payloads must support the derived trait.Generic structures and enumerations do not currently support these derives.Except for `#[test]` and `#[ignore]` in the next section, other attributes and `extern fn` are used for the compiler's own runtime, intrinsic and lang items; ordinary GoML projects cannot use this to bind any Go symbols.

Derived `Eq` provides `Eq::eq(left, right)`, making the type usable with `==` / `!=` and satisfying the key constraints of `HashMap`.

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
```

Test source code cannot be used to fix a production package that itself fails inspection.

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

It is recommended to always write `Option::Some`, `Option::None`, `Result::Ok` and `Result::Err` when constructing and matching.

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

The signatures of built-in key-related traits are `Eq::eq(self, other: Self) -> bool` and `Hash::hash(self) -> uint64`; user types can be handwritten impl, and non-generic structures and enumerations can also be generated using derive.

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
let values: Vec[int32] = Vec::new();
values.push(1);
values.push(2);
let first = values.get(0);
```

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
let counts: HashMap[string, int32] = HashMap::new();
counts.set("a", 1);
let value: Option[int32] = counts.get("a");
```

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

Commonly used APIs:

- `FnIterator::from_fn(next_fn)`
- `iterator.next()` or `Iterator::next(iterator)`
- `range(start, end)` or `start..end`, produces an incrementing half-open `FnIterator[int]`; `start >= end` is empty
- `iterator_map(iterator, fn)`
- `iterator_filter(iterator, predicate)`
- `iterator_take(iterator, count)`
- `iterator_fold(iterator, initial, combine)`
- `iterator_collect(iterator) -> Vec[T]`

Iterators are single pass.`Vec[T]` and `Slice[T]` can be directly used in `for`, and the value implementing `Iterator` is also directly iterable through the identity `IntoIterator`.

## Standard library package

The standard library is not equal to prelude and needs to be imported by package:

```goml
use std::bytes;
use std::collections;
use std::env;
use std::fs;
use std::io;
use std::json;
use std::path;
use std::process;
use std::testing;
use std::text;
```

Current public entrances include:

- `bytes::Bytes`, a mutable byte buffer with conversion to and from strings and `Vec[uint8]`
- `collections::Arena`, `BitSet`, `Deque`, `HashSet`, `IndexVec`, `Interner`, and `Stack`
- `env::args`, `current_dir`, `current_exe`, and `var`
- `fs::read_file`, `write_file`, byte I/O, directory operations, path inspection, and `sha256_file`
- `io::print`, `println`, `eprint`, `eprintln`, and byte-oriented standard stream I/O
- `json::Value`, `parse`, `encode`, `field`, and typed `as_*` accessors
- `path::join`, `clean`, `is_absolute`, component inspection, and `absolute`
- `process::Command`, `ExitStatus`, `Output`, `exit`, and `look_path`
- `testing::fail`, `assert`, `assert_eq`, and `assert_ne`
- `text::StringBuilder`

File system operations use `Result[..., string]` to report errors, and can be combined with `?`.

## Comparison of common writing errors

| Don't generate | GoML writing method |
| --- | --- |
| `Vec<int>` | `Vec[int]` |
| `fn id<T>(x: T) -> T` | `fn id[T](x: T) -> T` |
| `id::<int32>(1)` | `id::[int32](1)` |
| Ordinary function `id[int32](1)` | `id::[int32](1)`, or rely on parameter/result type inference |
| `1i32`、`1u64`、`1.0f32` | Use the expected type, such as `let value: uint64 = 1;` |
| `let mut x: &T` | Use value `T` or `Ref[T]` as required |
| Write `if cond { value }` in the value position | `if cond { value } else { other }` |
| `let Option::Some(x) = value;` | `if let Option::Some(x) = value { ... };` or `match` |
| `let Point { x } = point;` | `let Point { x, .. } = point;` |
| Endless `match` | Complete variant or `_` branch |
| `x += 1`、`x++` | `x = x + 1;` |
| `point.x = value` | Rebuild the structure or design the field as `Ref[T]` |
| `var x = 1`、`x := 1` | `let x = 1;` |
| `loop { ... }` | `while true { ... }` |
| `for i := 0; ...` | `while` , or `for i in range(start, end)` |
| `switch` | `match` |
| `null`、`nil` | `Option::None` |
| `throw`, exception | `Result` and `?` |
| `float_value as int32` | Floating point to integer conversion is not supported; use dedicated parsing or conversion APIs |
| `dyn A + B` | Single `dyn A`, or redesign the combined trait |
| `dyn TraitWithAssociatedType` | dyn traits cannot have type parameters or associated types |
| `use pkg::{A, B}` | Use `package::A` after `use full::package;` |
| `mod`、`crate::`、`super::` | Directory packages and canonical `use` paths |
| `fn helper` inside function | Top-level function or local closure |
| User `extern fn` | Use the API provided by compiler/prelude/stdlib |

## Informal Grammar Quick Facts

The following EBNF only describes the canonical form that should be generated; `?` means optional, `*` means repeated, and the terminator is placed in quotes.

```text
file          = package_decl? use_decl* item*
package_decl  = "package" lower_ident ";"
use_decl      = "use" path ("as" lower_ident)? ";"
path          = ident ("::" ident)*

item          = attribute* visibility? function
              | attribute* visibility? struct_def
              | attribute* visibility? enum_def
              | attribute* visibility? trait_def
              | attribute* impl_def
visibility    = "pub"
attribute     = "#[" attribute_body "]"

function      = "fn" lower_ident generic_params? param_list return_type? where_clause? block
method        = visibility? "fn" lower_ident param_list return_type? where_clause? block
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
              | "fn" lower_ident param_list return_type? ";"

impl_def      = "impl" generic_params? trait_ref "for" type where_clause?
                "{" impl_member* "}"
              | "impl" generic_params? type where_clause?
                "{" method* "}"
impl_member   = "type" upper_ident "=" type ";" | "fn" lower_ident param_list return_type? block

trait_set     = trait_ref ("+" trait_ref)*
trait_ref     = path type_args?
where_clause  = "where" where_predicate ("," where_predicate)* ","?
where_predicate = type ":" trait_set | type "=" type

type          = primitive_type
              | path type_args?
              | "dyn" path
              | "[" type ";" integer_literal "]"
              | "(" type_list ")"
              | "()" "->" type
              | type "->" type
type_args     = "[" type_list "]"
type_list     = type ("," type)* ","?

block         = "{" statement* expression? "}"
statement     = "let" "mut"? pattern (":" type)? "=" expression ";"
              | assign_target "=" expression ";"
              | expression ";"
              | control_expression

expression    = literal | path | tuple | array | struct_literal | closure
              | call | field | index | unary | binary | cast | range_expression
              | try_expression
              | if_expression | match_expression | while_expression | for_expression
              | "return" expression? | "break" | "continue" | "go" expression

control_expression = if_expression | match_expression | while_expression | for_expression
if_expression = "if" expression block ("else" (block | if_expression))?
              | "if" "let" pattern "=" expression block
                ("else" (block | if_expression))?
match_expression = "match" expression
                   "{" (match_arm ",")* match_arm? "}"
match_arm     = pattern ("if" expression)? "=>" (expression | block)
while_expression = "while" expression block
              | "while" "let" pattern "=" expression block
for_expression = "for" pattern "in" expression block
closure       = "||" (expression | block)
              | "|" closure_params? "|" (expression | block)
cast          = expression "as" integer_type
range_expression = expression ".." expression

pattern       = or_pattern
or_pattern    = alias_pattern ("|" alias_pattern)*
alias_pattern = ident "@" alias_pattern | range_pattern
range_pattern = primary_pattern ((".." | "..=") range_endpoint)?
range_endpoint = "-"? integer_literal | char_literal
primary_pattern = ident | "_" | literal | "-" numeric_literal | "()"
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
bin/stage1/gomlc run-single path/to/main.gom
```

For a project, run the installed driver from anywhere inside its module:

```sh
goml check
goml build
goml check --tests
goml test
goml run
```

`goml check`, `goml build`, and `goml test` always operate on the complete module and do not accept package or file targets. `goml run [TARGET]` accepts an optional entry package file or directory when a module has multiple executable packages.

When you need to inspect a compilation phase, add `--dump-ast`, `--dump-hir`, `--dump-tast`, `--dump-core`, `--dump-mono`, `--dump-lift`, `--dump-anf`, or `--dump-go` to `gomlc run-single`.

The code agent should at least run the corresponding `goml check` or `gomlc run-single` before submitting the source code; when modifying the test, it should also run `goml check --tests` and the related `goml test`.When type inference fails, give priority to adding local result types, empty container types, closure parameter types, or using UFCS instead of rewriting to unsupported Rust/Go syntax.
