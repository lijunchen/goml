# GoML Language Specification

Status: Implementation-oriented specification aligned with current compiler behavior in `crates/parser` and `crates/compiler/src/typer`.

This document describes GoML as implemented today, not an aspirational design. When implementation behavior and intuition differ, implementation behavior is normative.

## 1. Scope and Conformance

1. This specification defines:
- Surface syntax accepted by the parser (`crates/parser/src`).
- Static semantics and type checking behavior implemented in typer (`crates/compiler/src/typer`).
- Error recovery and diagnostic behavior that affects user-visible language semantics.

2. This specification does not define:
- Concrete lexer regexes beyond token categories consumed by the parser.
- Runtime scheduling guarantees for concurrency.
- Full backend code generation details beyond semantics relevant to typing and dispatch.

3. Normative terms:
- MUST: required by current implementation.
- SHOULD: strongly recommended usage pattern.
- MAY: allowed by implementation.

## 2. Implementation Pipeline Context

The compiler frontend lowers source through:

`lexer -> parser -> CST -> AST -> HIR -> typed AST (TAST) -> Core -> Mono -> Lift -> ANF -> Go`

This document focuses on parser and typer behavior.

## 3. Lexical Tokens and Trivia (Parser-Visible)

## 3.1 Trivia

The parser input stream skips trivia tokens:
- `Whitespace`
- `Comment`

Trivia does not participate in grammar decisions.

## 3.2 Keywords (Consumed or Reserved by Parser)

Top-level and declarations:
- `package`, `use`, `import`, `extern`, `fn`, `enum`, `struct`, `trait`, `impl`, `for`, `type`

Control and statement-related tokens:
- `if`, `else`, `match`, `while`, `let`, `go`, `return`

Type and trait-object related:
- `unit`, `bool`, `int8`, `int16`, `int32`, `int64`
- `uint8`, `uint16`, `uint32`, `uint64`
- `float32`, `float64`, `string`, `char`, `dyn`

Literals and wildcard keywords:
- `true`, `false`, `_`

Notes:
- `import` token exists but `import` syntax is explicitly rejected (removed feature).
- `return` token exists but there is currently no `return` statement syntax in parser rules.

## 3.3 Literal Tokens

Parser recognizes literal token classes:
- Signed/unsuffixed integers: `int`, `int8_lit`, `int16_lit`, `int32_lit`, `int64_lit`
- Unsigned integers: `uint8_lit`, `uint16_lit`, `uint32_lit`, `uint64_lit`
- Floating-point: `float`, `float32_lit`, `float64_lit`
- Strings: `str`, `multiline_str`
- Character: `char_lit`
- Identifier: `ident`

## 3.4 Punctuation and Operators

Delimiters and punctuation:
- `(` `)` `{` `}` `[` `]` `;` `,` `:` `::` `.` `#`

Arrows:
- `->`, `=>`

Operators:
- Arithmetic: `+ - * /`
- Logical: `! && ||`
- Comparison: `== != < > <= >=`
- Closure separator: `|`, `||`

## 4. Parsing Model and Recovery

## 4.1 Event-Based Parsing

The parser builds CST via an event stream (`Open`, `Close`, `Advance`, `Error`).

## 4.2 `expect` Recovery Behavior

When `expect(token)` fails:
1. If current token belongs to non-consumable recovery set, parser records error and does not advance.
2. Otherwise parser emits an `ErrorTree` and consumes one token.

Non-consumable recovery set includes:
- Top-level starters (`fn`, `extern`, `struct`, `enum`, `trait`, `impl`, `package`, `use`, `import`)
- Statement starters (`let`, `return`)
- Closers/separators (`)`, `]`, `}`, `;`, `,`)

## 4.3 Fuel Safety

Parser has a fuel mechanism to avoid non-progress loops. If parse logic repeatedly fails to consume input, parsing aborts with an internal parser-progress diagnostic.

## 4.4 Block and Parameter Recovery Sets

Implementation-level recovery sets include:
- Parameter list recovery around `->`, `{`, `fn`
- Block recovery around top-level starters (`#`, `fn`, `extern`, `struct`, `enum`, `trait`, `impl`, `package`, `use`, `import`)

## 5. Concrete Grammar (EBNF-Like)

This grammar reflects parser behavior, including permissive/recoverable areas.

## 5.1 File Structure

```ebnf
File           ::= PackageDecl? UseDecl* TopLevel* EOF
TopLevel       ::= AttributedItem | Item | Expr
AttributedItem ::= Attribute+ Item
PackageDecl    ::= "package" Ident ";"
UseDecl        ::= "use" Path ";"
```

Rules:
- `package` MUST appear only at file top.
- `use` MUST appear only after optional `package` and before other top-level elements.
- Violations are diagnosed and parsing continues.
- `import` is diagnosed as removed syntax.
- Top-level expressions are parser-accepted.

## 5.2 Attributes

```ebnf
Attribute      ::= "#" "!"? "[" AttributeBody "]"
AttributeBody  ::= (AnyTokenExceptUnbalancedBracket | AttributeNestedBracket)*
AttributeNestedBracket ::= "[" AttributeBody "]"
```

Rules:
- Attribute brackets are balance-counted.
- Unterminated attribute body is diagnosed.
- Attributes MUST attach to a top-level item. Otherwise parser emits an error node.

## 5.3 Paths

```ebnf
Path           ::= ("::")? Ident ("::" Ident)*
```

Rules:
- `::` without following identifier is diagnosed.
- Missing initial identifier is diagnosed.
- Path nodes are always wrapped in type, constructor pattern, and struct-literal contexts.

## 5.4 Types

```ebnf
Type           ::= TypeArrow
TypeArrow      ::= TypeAtom ("->" TypeArrow)?
TypeAtom       ::= PrimitiveType
                | "dyn" Path
                | "[" Type ";" IntLiteral "]"
                | "(" TypeList? ")"
                | Path TypeArgs?
TypeList       ::= Type ("," Type)*
TypeArgs       ::= "[" Type ("," Type)* "]"
PrimitiveType  ::= "unit" | "bool"
                | "int8" | "int16" | "int32" | "int64"
                | "uint8" | "uint16" | "uint32" | "uint64"
                | "float32" | "float64"
                | "string" | "char"
```

Rules:
- Function type arrow is right-associative.
- `(` `)` type syntax produces tuple-type CST node form; there is no dedicated paren-type node.
- Array length in syntax is integer literal token.

## 5.5 Generic Parameter and Trait-Bound Syntax

```ebnf
GenericList    ::= "[" GenericParam ("," GenericParam)* "]"
GenericParam   ::= Ident NestedGenericList? (":" TraitSet)?
NestedGenericList ::= "[" GenericParam ("," GenericParam)* "]"
TraitSet       ::= Path ("+" Path)*
```

Rules:
- Trait bounds are syntax-enabled only in `fn` and `impl` generic positions.
- `struct`, `enum`, `trait` generic lists do not accept bounds in parser mode.

## 5.6 Top-Level Items

```ebnf
Item           ::= ExternItem
                | FnItem
                | EnumItem
                | StructItem
                | TraitItem
                | ImplItem

FnItem         ::= "fn" Ident GenericList? "(" ParamList? ")" ("->" Type)? Block
ParamList      ::= Param ("," Param)*
Param          ::= Ident ":" Type

EnumItem       ::= "enum" Ident GenericList? "{" EnumVariantList? "}"
EnumVariantList ::= EnumVariant ("," EnumVariant)* ","?
EnumVariant    ::= Ident ("(" TypeList? ")")?

StructItem     ::= "struct" Ident GenericList? "{" StructFieldList? "}"
StructFieldList ::= StructField ("," StructField)* ","?
StructField    ::= Ident ":" Type

TraitItem      ::= "trait" Ident GenericList? "{" TraitMethodSigList? "}"
TraitMethodSigList ::= TraitMethodSig (";" TraitMethodSig)* ";"?
TraitMethodSig ::= "fn" Ident "(" TypeList? ")" ("->" Type)?

ImplItem       ::= "impl" GenericList? ImplHead "{" FnItem* "}"
ImplHead       ::= Path "for" Type
                | Type
```

Rules:
- `impl` head is disambiguated by lookahead for `Path ... for`.
- Trait methods declared in `trait` are signature-only.
- Impl body methods parse like regular `fn` items.

## 5.7 `extern` Forms

```ebnf
ExternItem     ::= "extern" ExternBody
ExternBody     ::= "type" Ident
                | "fn" Ident GenericList? "(" ParamList? ")" ("->" Type)?
                | StringLiteral StringLiteral StringLiteral? ExternForeignDecl
ExternForeignDecl ::= "type" Ident
                   | Ident "(" ParamList? ")" ("->" Type)?
```

Rules:
- Standard foreign form requires language string and package string.
- Optional third string is explicit foreign symbol name.
- Missing required strings/names produce diagnostics.

## 5.8 Statements and Blocks

```ebnf
Block          ::= "{" BlockElement* "}"
BlockElement   ::= LetStmt
                | ExprStmt
                | TailExpr
                | ";"
LetStmt        ::= "let" Pattern (":" Type)? "=" Expr ";"
ExprStmt       ::= Expr ";"
TailExpr       ::= Expr
```

Tail-expression rule:
- First expression in block without trailing semicolon becomes block tail expression.
- After tail expression appears, any further non-`}` token is diagnosed as invalid continuation.

## 5.9 Expressions

```ebnf
Expr           ::= PrattExpr
PrattExpr      ::= PrefixExpr (InfixOp PrefixExpr | CallSuffix | FieldSuffix)*
PrefixExpr     ::= PrefixOp PrefixExpr | AtomExpr
PrefixOp       ::= "-" | "!"
CallSuffix     ::= "(" ArgList? ")"
ArgList        ::= Expr ("," Expr)*
FieldSuffix    ::= "." Ident
InfixOp        ::= "||" | "&&"
                | "==" | "!="
                | "<" | ">" | "<=" | ">="
                | "+" | "-" | "*" | "/" | "."
```

Atom expressions:

```ebnf
AtomExpr       ::= Literal
                | PathOrStructLiteral
                | ArrayLiteral
                | TupleOrParenOrUnit
                | IfExpr
                | MatchExpr
                | WhileExpr
                | ClosureExpr
                | GoExpr

PathOrStructLiteral ::= Path StructLiteralTail?
StructLiteralTail   ::= "{" StructLiteralFieldList? "}"
StructLiteralFieldList ::= StructLiteralField ("," StructLiteralField)* ","?
StructLiteralField ::= Ident (":" Expr)?

ArrayLiteral   ::= "[" (Expr ("," Expr)*)? "]"
TupleOrParenOrUnit ::= "(" ")"
                    | "(" Expr ")"
                    | "(" Expr "," ExprListTail? ")"
ExprListTail   ::= Expr ("," Expr)* ","?

IfExpr         ::= "if" Expr (Block | Expr) "else" (Block | Expr)
MatchExpr      ::= "match" Expr "{" MatchArmList? "}"
MatchArmList   ::= MatchArm ("," MatchArm)* ","?
MatchArm       ::= Pattern "=>" (Block | Expr)
WhileExpr      ::= "while" Expr (Block | Expr)
GoExpr         ::= "go" Expr

ClosureExpr    ::= "||" ClosureBody
                | "|" ClosureParamList? "|" ClosureBody
ClosureParamList ::= ClosureParam ("," ClosureParam)*
ClosureParam   ::= Ident (":" Type)?
ClosureBody    ::= Block | Expr
```

## 5.10 Patterns

```ebnf
Pattern        ::= LiteralPattern
                | WildPattern
                | VarPattern
                | TuplePattern
                | ConstructorPattern

LiteralPattern ::= BoolLiteral
                 | NumberLiteral
                 | StringLiteral
                 | CharLiteral
WildPattern    ::= "_"
VarPattern     ::= Ident
TuplePattern   ::= "(" ")"
                | "(" Pattern "," PatternListTail? ")"
PatternListTail ::= Pattern ("," Pattern)* ","?

ConstructorPattern ::= Path ConstructorPayload?
ConstructorPayload ::= "(" PatternList? ")"
                     | "{" StructPatternFieldList? "}"
StructPatternFieldList ::= StructPatternField ("," StructPatternField)* ","?
StructPatternField ::= Ident (":" Pattern)?
```

Disambiguation:
- Bare `ident` is variable pattern only when not followed by `::`, `(`, `{`.
- Otherwise it is constructor/path pattern.

## 6. Operator Precedence and Associativity

Parser uses Pratt binding powers.

From low to high precedence:

1. `||` (left-associative)
2. `&&` (left-associative)
3. `==`, `!=` (left-associative)
4. `<`, `>`, `<=`, `>=` (left-associative)
5. `+`, `-` (left-associative)
6. `*`, `/` (left-associative)
7. Call suffix `(...)`
8. Field access `.` and prefix unary `-`, `!` (high)

Selected consequences:
- `a.b()` parses as `(a.b)()`.
- `-a.b` parses as `-(a.b)`.

## 7. Name Resolution Semantics (AST -> HIR)

Name resolution is implemented in `name_resolution.rs` and is recoverable: unresolved or ambiguous cases emit diagnostics and continue.

## 7.1 Pre-Scan and Definition Indexing

Before expression-level resolution:
- Compiler pre-registers top-level definitions with `DefId`.
- Builtin names are indexed separately.
- Enum constructors are indexed for constructor-path normalization.
- Trait names are indexed for trait-lookup and ambiguity checks.

## 7.2 Resolution Priority

For single-segment path `x` in expression position:
1. Local scope binding
2. Current package top-level definition
3. Builtin definition
4. Unresolved name diagnostic

For multi-segment paths:
- Attempt direct package/def resolution first.
- If unresolved and syntactically member-like, lower to static-member expression form for typer (`Type::member`, `Trait::method` handling).

## 7.3 Import Visibility Rules

For explicit package-prefixed names:
- Access to non-current package requires package to be imported (except builtin package paths).
- Missing import emits diagnostic and continues with error-tolerant lowering.

## 7.4 Constructor Path Normalization

Resolution rules for enum variants:
- `Variant`:
  - If unique variant name among enums in scope/package index, normalize to `Enum::Variant`.
  - If ambiguous across enums, keep unresolved/ambiguous form and diagnose later.
- `Enum::Variant`: resolve in current package, fallback builtin package.
- `Package::Enum::Variant`: package must be imported/allowed.

## 7.5 Trait Name Ambiguity

For trait names in impl heads and type contexts:
- If unqualified trait name conflicts between local and builtin trait, compiler emits ambiguity diagnostic and requires explicit package qualification.

## 8. Type System Core Concepts

Typer lowers HIR to TAST and generates/solves constraints.

Type forms (as represented in typer/TAST):
- Primitive scalar types
- Unit and tuple
- Function type
- Array with length
- User nominal/app types
- Type parameters (`TParam`)
- Type variables (`TVar`) for inference
- Builtin containers specialized in type representation:
  - `Ref[T]`
  - `Vec[T]`
  - `Slice[T]`
  - `HashMap[K, V]`
- Trait object type `dyn Trait`

## 8.1 Type Validation

`validate_ty` and related utilities enforce:
- Every referenced type parameter must be declared in current generic environment.
- Type names and trait names must resolve.
- `dyn Trait` must satisfy object-safety restrictions (see Section 11).

## 8.2 Builtin Type Lowering Special Cases

During HIR type conversion:
- `Ref`, `Vec`, `Slice`, `HashMap` are recognized by name and lowered to dedicated internal type nodes, not generic `TApp` fallback.

## 9. Expression Typing Rules (HIR -> TAST)

Typing is split between inference mode and check-against-expected mode.

## 9.1 Literals

- Integer and float literals are range-checked according to target type context.
- Char literals are decoded and validated.
- Invalid literal values emit diagnostics and use recoverable placeholder values.

## 9.2 Arithmetic and Logical Operators

- Arithmetic operators constrain operands/results to numeric-compatible types.
- Logical operators require boolean operands.
- Comparison operators generate equality/order constraints as required.

## 9.3 Function Calls

Call typing flow:
1. Infer callee type.
2. Instantiate generic scheme (`inst_ty`) by replacing type parameters with fresh type variables.
3. Check argument count and types.
4. Accumulate trait-bound constraints from function scheme.

If callee is unresolved single identifier, typer attempts unqualified function scheme lookup before final failure.

## 9.4 Method Calls `x.method(...)`

Lookup order:
1. Inherent methods on receiver type.
2. Trait methods that are in scope and implemented for receiver.

Special behavior:
- For receiver `TParam`, trait method candidate search uses bounds on that type parameter.
- For non-`TParam`, trait methods considered only among in-scope traits with visible impls.
- Ambiguous candidates produce diagnostic and require UFCS disambiguation.

## 9.5 Static Member Calls `TypeOrTrait::member(...)`

Typer distinguishes:
- Type-associated/inherent members.
- Trait UFCS calls.
- Dyn trait UFCS path (`DynTraitMethod`) when receiver expression is `dyn Trait`.

## 9.6 Closures

Inference behavior:
- Parameter type from annotation if present; otherwise fresh type variable.
- Body inferred, closure type becomes `(params...) -> ret`.
- Captured locals are tracked via closure-specific local environment transitions.

Check behavior:
- If expected type is function type with matching arity, closure params and body are checked against expected signature.

## 9.7 `let`, Blocks, and Tail Expressions

- `let` expression/statement type is `unit`.
- Pattern and initializer are type-checked; bindings are introduced into local env.
- `let` pattern MUST be irrefutable; otherwise diagnostic is emitted and compilation continues.
- Block type is type of tail expression when present, otherwise `unit`.

## 9.8 `if`, `match`, `while`, `go`

- `if`: condition must be `bool`; then/else branches are type-unified.
- `match`: scrutinee and arm patterns are checked; arm result types unified.
- `while`: condition must be `bool`; loop expression type is `unit`.
- `go e`: `e` must type-check as zero-arg function `() -> unit`.

## 9.9 Struct Literals and Field Access

- Struct literal typing validates field existence and field type compatibility.
- Missing/extra fields produce diagnostics with recoverable placeholders in elaboration.
- Field access may be solved immediately or deferred using `StructFieldAccess` constraints.

## 10. Pattern Typing Rules

`check_pat` enforces pattern-shape-specific typing.

Pattern categories:
- Wildcard: accepts expected type.
- Variable pattern: binds variable with expected type.
- Literal patterns: expected type constrained to literal type.
- Tuple/unit patterns: arity and component type checks.
- Constructor patterns:
  - Resolve constructor kind (enum variant / struct constructor context).
  - Validate payload arity or named fields.
  - Bind nested subpatterns recursively.

## 10.1 Irrefutability in `let`

`let` rejects refutable patterns with diagnostic:
- Typical rejected forms: enum variant patterns such as `Some(x)`.
- Recoverable behavior: typed `ELet` remains in TAST with emitted error.

## 11. Traits, Impls, and Dynamic Dispatch

## 11.1 Trait Definition Semantics

- Trait methods are signature declarations.
- Trait method schemes are stored in global type environment.

## 11.2 Trait Impl Rules

Trait impl checking enforces:
1. Orphan rule: trait or target type must be local.
2. Method completeness: all required trait methods must be implemented.
3. No duplicate methods in impl block.
4. Signature equality with trait declaration (parameter and return types must match exactly).
5. No duplicate trait impl for same trait-target instance.

## 11.3 Inherent Impl Rules

- Inherent impl target must be local nominal type (or recognized builtin key path as implemented).
- Inherent methods are registered and preferred in method lookup over trait methods.

## 11.4 Trait Method Resolution and Ambiguity

For `x.method(...)`:
- Compiler first tries inherent method.
- Then trait method candidates under current trait-in-scope constraints.
- Multiple candidates cause ambiguity diagnostic; UFCS required.

For `Trait::method(x, ...)`:
- Treated as explicit trait dispatch.
- Enables disambiguation and dyn dispatch path where applicable.

## 11.5 `dyn Trait` Object Safety and Calls

`dyn Trait` validation (`validate_dyn_trait`) requires:
1. Each trait method has receiver in first parameter position.
2. First parameter type is exactly `Self`.
3. `Self` MUST NOT appear in non-receiver parameters.
4. `Self` MUST NOT appear in return type.

Dispatch behavior:
- `Trait::method(x_dyn, ...)` is supported for `x_dyn: dyn Trait`.
- `x_dyn.method(...)` via dot syntax does not resolve in current implementation and typically emits method-not-found diagnostic.

## 12. Generic Semantics and Inference Constraints

## 12.1 Generic Instantiation

At each generic call site:
- Function/method scheme type parameters are instantiated into fresh type variables.
- Type constraints are generated to connect call arguments/results with instantiated signature.

## 12.2 Trait Bounds Application

Function and method generic constraints (`T: Trait`) are converted into implementability constraints during call checking.

## 12.3 Type Parameter Unification Behavior

Important current rule in unifier:
- `TParam` only unifies with same-named `TParam`.
- `TParam` does not directly unify with concrete type in unifier.

Observable consequence:
- Expected-type contexts do not rewrite generic parameter symbols into concrete types via `TParam` unification path.

## 13. Constraint System and Solver

Typer generates constraints and solves to fixpoint.

Constraint kinds:
1. `TypeEqual(left, right, origin)`
2. `Overloaded { op, trait_name, call_site_type }`
3. `Implements { trait_name, for_ty }`
4. `StructFieldAccess { expr_ty, field, result_ty }`

## 13.1 Solving Strategy

- Iterate constraints until no progress.
- Defer constraints requiring concrete type information.
- Emit diagnostics when constraints remain unsatisfied after stabilization.

## 13.2 `Overloaded` Resolution

When receiver type is concrete:
- Collect visible trait impl schemes.
- Zero candidates: emit missing-impl diagnostic.
- Multiple candidates: emit ambiguity diagnostic.
- Exactly one candidate: inject corresponding type-equality constraints.

## 13.3 `Implements` Resolution

Constraint satisfied when:
- Type is `dyn Trait` compatible with requested trait, or
- Concrete type has visible trait impl.

If type not concrete yet, constraint may be deferred.

## 13.4 Field Access Resolution

`StructFieldAccess` constraints:
- Resolve struct field type under potential generic substitution.
- Delay until structure type is sufficiently concrete.

## 13.5 Unification Rules

Unifier handles:
- Type variable binding with occurs-check.
- Structural recursion over tuples/functions/type applications/builtin containers.
- Array length compatibility with wildcard allowance in implemented representation.
- Failure diagnostics for incompatible shapes.

Occurs-check failure emits recursive-type diagnostic.

## 13.6 Unresolved Type Variable Diagnostics

During substitution/finalization:
- Unresolved type variables generate `Could not infer type` diagnostics.
- Diagnostic origin attempts to point to source of unresolved inference variable.

## 14. Builtin Types, Traits, and Functions

This section summarizes currently implemented builtins as seen by parser+typer and tests.

## 14.1 Builtin Containers

`Ref[T]`:
- Construct: `ref(x)`
- Access: `ref_get(r)`
- Update: `ref_set(r, v)`
- Supports nesting.

`Vec[T]`:
- `vec_new()`, `vec_push(v, x)`, `vec_get(v, i)`, `vec_len(v)`
- Method-style APIs are also supported through builtin registrations.

`Slice[T]`:
- `slice(vec, start, end)`
- `slice_get(s, i)`, `slice_len(s)`, `slice_sub(s, start, end)`
- Read-only view type behavior.

`HashMap[K, V]`:
- `hashmap_new`, `hashmap_get`, `hashmap_set`, `hashmap_remove`, `hashmap_len`, `hashmap_contains`
- Key type semantics rely on `Eq` and `Hash` traits.

## 14.2 Builtin Trait Notes

- `Eq` and `Hash` are builtin traits in scope model.
- `Ref[T]` equality/hash behavior is content-oriented in current builtin semantics (not pointer-identity).
- Pointer identity can be compared with dedicated builtin (`ptr_eq`) as shown in tests.

## 14.3 Builtin Function Registration

Builtin extern-like declarations are registered into global schemes; name resolution may bind single-segment names directly to builtin IDs.

## 15. Control Flow and Evaluation-Oriented Semantics

## 15.1 `if`

- Expression form.
- Requires both then and else branches in parser grammar.
- Branch result types must unify.

Example:

```gom
fn classify(x: int32) -> string {
    if x > 0 {
        "positive"
    } else {
        "non-positive"
    }
}
```

## 15.2 `match`

- Expression form with ordered arms.
- Arm order is semantically relevant.
- Pattern checks are typed against scrutinee.
- Arm result types must unify.

Example:

```gom
enum Expr { Zero, Add(int32, int32), Mul(Expr, Expr) }

fn eval(e: Expr) -> int32 {
    match e {
        Expr::Zero => 0,
        Expr::Add(x, y) => x + y,
        Expr::Mul(_, Expr::Zero) => 0,
        Expr::Mul(_, _) => 1,
    }
}
```

## 15.3 `while`

- Expression form producing `unit`.
- Condition must be `bool`.

Example:

```gom
fn sum_to(n: int32) -> int32 {
    let i = ref(0);
    let acc = ref(0);
    while ref_get(i) < n {
        ref_set(acc, ref_get(acc) + ref_get(i));
        ref_set(i, ref_get(i) + 1);
    }
    ref_get(acc)
}
```

## 15.4 `go`

- Spawns concurrent evaluation of expression.
- Typed as requiring `() -> unit` closure/function expression.

Example:

```gom
fn main() -> unit {
    let done = ref(false);
    go || {
        ref_set(done, true);
    };
    while !ref_get(done) {}
}
```

## 16. Error Model and Diagnostics

GoML parser and typer are explicitly recoverable. Errors are accumulated rather than aborting on first failure.

## 16.1 Parser Diagnostics (Representative)

- Top-level order violations: `package`/`use` outside allowed region.
- Removed syntax: `import` is no longer supported.
- Missing expected tokens: identifier, `:`, `=`, `;`, delimiters, arrow tokens.
- Unterminated attribute body.
- Block closure failures:
  - `expected '}' after block expression`
  - `expected '}' to close block`

## 16.2 Typer Diagnostics (Representative)

- Unresolved names/types/traits.
- Package not imported.
- Method not found for receiver type.
- Ambiguous method resolution (UFCS required).
- Trait impl rule violations:
  - orphan rule
  - missing method
  - duplicate method
  - signature mismatch
- `dyn` object safety violations.
- Type mismatch and unification failures.
- Unresolved inference variables (`Could not infer type`).
- Refutable `let` pattern diagnostic.

## 16.3 Recovery Strategy

- Name resolution: unresolved symbols lowered into error-tolerant HIR variants.
- Typing: erroneous subexpressions replaced with error placeholders where needed.
- Constraint solving proceeds as far as possible before final unresolved reports.
- No panic-based ambiguity handling in lookup paths; ambiguous cases become diagnostics.

## 17. Current Known Behavioral Boundaries

1. `return` token exists but there is no return-statement syntax.
2. `import` syntax is removed and diagnosed.
3. Type parentheses do not create a dedicated paren-type node; tuple node form is used.
4. `dyn Trait` dot-call (`x.method()`) does not resolve in current implementation; UFCS call is the supported route.
5. Generic parameter unification is conservative (`TParam` only with same `TParam`).
6. `let` requires irrefutable patterns; refutable forms are diagnosed.

## 18. Worked Examples

## 18.1 Struct, Enum, Pattern Matching

```gom
struct Point {
    x: int32,
    y: int32,
}

enum Shape {
    Dot(Point),
    Empty,
}

fn score(s: Shape) -> int32 {
    match s {
        Shape::Dot(Point { x, y }) => x + y,
        Shape::Empty => 0,
    }
}
```

## 18.2 Trait, Impl, UFCS Disambiguation

```gom
trait A { fn pick(Self) -> int32; }
trait B { fn pick(Self) -> int32; }

struct S { v: int32 }

impl A for S {
    fn pick(self: S) -> int32 { self.v }
}

impl B for S {
    fn pick(self: S) -> int32 { self.v + 10 }
}

fn main() -> unit {
    let s = S { v: 5 };
    string_println(int32_to_string(A::pick(s)));
}
```

## 18.3 Dynamic Trait Object and UFCS Call

```gom
trait Display {
    fn to_string(Self) -> string;
}

struct N { v: int32 }

impl Display for N {
    fn to_string(self: N) -> string {
        int32_to_string(self.v)
    }
}

fn show(x: dyn Display) -> string {
    Display::to_string(x)
}
```

## 18.4 Generic Function with Bound

```gom
trait Show {
    fn show(Self) -> string;
}

fn render[T: Show](x: T) -> string {
    Show::show(x)
}
```

## 18.5 `HashMap` and `Vec` Builtins

```gom
fn main() -> unit {
    let m = hashmap_new[string, int32]();
    hashmap_set(m, "a", 1);
    let v = vec_new[int32]();
    vec_push(v, 10);
    vec_push(v, 20);
    string_println(int32_to_string(vec_len(v)));
}
```

## 18.6 `Slice` Builtin

```gom
fn main() -> unit {
    let v = vec_new[int32]();
    vec_push(v, 10);
    vec_push(v, 20);
    vec_push(v, 30);
    let s = slice(v, 1, 3);
    string_println(int32_to_string(slice_get(s, 0)));
}
```

## 19. Source-to-Spec Coverage Map

## 19.1 Parser Files Coverage

- `crates/parser/src/lib.rs`: parse entry, CST build integration.
- `crates/parser/src/syntax.rs`: syntax/token inventory reflected in Sections 3 and 5.
- `crates/parser/src/input.rs`: trivia skipping reflected in Section 3.1.
- `crates/parser/src/event.rs`: event model reflected in Section 4.1.
- `crates/parser/src/parser.rs`: `expect` recovery/fuel behavior in Sections 4.2/4.3.
- `crates/parser/src/error.rs`: parser diagnostics summarized in Section 16.1.
- `crates/parser/src/path.rs`: path grammar and diagnostics in Section 5.3.
- `crates/parser/src/file.rs`: top-level items, types, generics, extern, block in Section 5.
- `crates/parser/src/expr.rs`: expression grammar and precedence in Sections 5.9 and 6.
- `crates/parser/src/stmt.rs`: let and expr statements in Section 5.8.
- `crates/parser/src/pattern.rs`: full pattern grammar in Section 5.10.

## 19.2 Typer Files Coverage

- `crates/compiler/src/typer/mod.rs`: typer orchestration in Sections 8 and 13.
- `crates/compiler/src/typer/name_resolution.rs`: Sections 7 and 16.3.
- `crates/compiler/src/typer/toplevel.rs`: Sections 11 and 12.
- `crates/compiler/src/typer/check.rs`: Sections 9, 10, 11.4, 11.5.
- `crates/compiler/src/typer/localenv.rs`: closure capture and local scope behavior in Section 9.6.
- `crates/compiler/src/typer/unify.rs`: unification details in Section 13.5.
- `crates/compiler/src/typer/util.rs`: type/trait resolution and dyn validation in Sections 8.1 and 11.5.
- `crates/compiler/src/typer/results.rs`: elaboration and coercion recording reflected across Sections 9 and 16.3.
- `crates/compiler/src/typer/tast_builder.rs`: TAST materialization and coercion application reflected in Sections 8 and 16.3.

## 20. Test-Grounded Behavioral Notes

The following behaviors are directly validated by pipeline tests:

1. Nested closures and captured state via `Ref` work, including multi-level nesting.
2. Higher-order function values are first-class.
3. Generic impl and trait-bound calls operate across instantiations.
4. Trait method ambiguity requires UFCS disambiguation.
5. Inherent and trait same-name methods can coexist with explicit selection.
6. `dyn Trait` values support UFCS dispatch and conversion from bounded type parameters.
7. `match` follows first-match arm order.
8. Builtin containers `Vec`, `Slice`, `Ref`, `HashMap` and associated APIs are type-checked and operational.
9. `go` launches concurrent computation and is typically synchronized through shared references.

This concludes the implementation-aligned GoML language specification for current parser+typer behavior.
