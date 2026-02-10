# GoML 语言规范（实现对齐版）

版本状态：本规范以当前编译器实现为准（语法以 parser 行为为准，语义以 typer 行为为准）。

本文目标：给出可执行、可落地的语言定义，尤其细化类型系统。

---

## 1. 规范范围

本规范覆盖：

1. 词法与语法（源代码能否被接受，以及如何被解析）。
2. 名称解析（标识符、路径、包导入、构造器、trait 名称解析）。
3. 类型系统（类型构成、推断、约束生成、统一、trait/impl/dyn、模式类型检查）。
4. 错误模型（可恢复诊断、失败条件、边界行为）。

本规范不覆盖：

1. Go 后端代码生成细节。
2. 运行时调度策略的强保证（尤其并发执行时序）。

---

## 2. 术语

1. 源语法树：CST/AST。
2. HIR：高层中间表示（完成名称解析后）。
3. TAST：带类型的 AST（完成类型推断/检查后）。
4. 类型变量（TVar）：推断中临时未知类型。
5. 类型参数（TParam）：泛型参数名，例如 `T`。
6. 约束（Constraint）：类型系统在检查过程中累积的等式或实现关系。

---

## 3. 词法与文件结构

## 3.1 Trivia

空白与注释在 parser 输入层被自动跳过。

## 3.2 关键字与符号（语法层可见）

关键字包含（按功能分组）：

1. 顶层与声明：`package`, `use`, `import`, `extern`, `fn`, `enum`, `struct`, `trait`, `impl`, `for`, `type`。
2. 控制流与语句：`if`, `else`, `match`, `while`, `let`, `go`, `return`。
3. 类型关键字：`unit`, `bool`, `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64`, `string`, `char`, `dyn`。
4. 其它：`true`, `false`, `_`。

注意：

1. `import` token 仍存在，但语法上被明确拒绝（已移除语法）。
2. `return` token 仍存在，但当前语法中没有 `return` 语句规则。

## 3.3 字面量 token 类别

1. 整数：`int`, `int8_lit`, `int16_lit`, `int32_lit`, `int64_lit`。
2. 无符号整数：`uint8_lit`, `uint16_lit`, `uint32_lit`, `uint64_lit`。
3. 浮点：`float`, `float32_lit`, `float64_lit`。
4. 字符串：`str`, `multiline_str`。
5. 字符：`char_lit`。
6. 标识符：`ident`。

## 3.4 文件结构约束

一个文件的顶层顺序：

1. 可选 `package`。
2. 若干 `use`。
3. 顶层项（`fn/enum/struct/trait/impl/extern`）及其属性。

顺序违规会报错，但 parser 会继续恢复。

---

## 4. 语法规则（实现等价）

以下为实现等价的 EBNF 风格描述。

## 4.1 文件与顶层

```ebnf
File           ::= PackageDecl? UseDecl* TopLevel* EOF
PackageDecl    ::= "package" Ident ";"
UseDecl        ::= "use" Path ";"
TopLevel       ::= AttrItem | Item | Expr
AttrItem       ::= Attribute+ Item
Item           ::= ExternItem | FnItem | EnumItem | StructItem | TraitItem | ImplItem
```

## 4.2 属性

```ebnf
Attribute      ::= "#" "!"? "[" AttributeBody "]"
AttributeBody  ::= balanced-tokens
```

属性只能附着在顶层项上；孤立属性会报错。

## 4.3 路径

```ebnf
Path           ::= ("::")? Ident ("::" Ident)*
```

## 4.4 类型表达式

```ebnf
Type           ::= TypeAtom ("->" Type)?          // 右结合
TypeAtom       ::= PrimitiveType
                | "dyn" Path
                | "(" TypeList? ")"
                | "[" Type ";" IntLiteral "]"
                | Path TypeArgs?
TypeList       ::= Type ("," Type)*
TypeArgs       ::= "[" Type ("," Type)* "]"
```

注意：

1. `->` 右结合。
2. `()` 与 `(T, U)` 都归入 tuple 语法族；没有独立 paren-type 节点。

## 4.5 泛型参数与 bound

```ebnf
GenericList    ::= "[" GenericParam ("," GenericParam)* "]"
GenericParam   ::= Ident (":" TraitSet)?
TraitSet       ::= Path ("+" Path)*
```

实现行为：

1. `fn` 与 `impl` 的泛型参数支持 `: Trait + Trait` bound。
2. `struct/enum/trait` 的泛型参数仅类型参数，不在该语法位接受 bound。

## 4.6 顶层声明

```ebnf
FnItem         ::= "fn" Ident GenericList? "(" ParamList? ")" ("->" Type)? Block
ParamList      ::= Param ("," Param)*
Param          ::= Ident ":" Type

EnumItem       ::= "enum" Ident GenericList? "{" EnumVariantList? "}"
EnumVariant    ::= Ident ("(" TypeList? ")")?

StructItem     ::= "struct" Ident GenericList? "{" StructFieldList? "}"
StructField    ::= Ident ":" Type

TraitItem      ::= "trait" Ident GenericList? "{" TraitMethodSigList? "}"
TraitMethodSig ::= "fn" Ident "(" TypeList? ")" ("->" Type)?

ImplItem       ::= "impl" GenericList? (Path "for" Type | Type) "{" FnItem* "}"
```

## 4.7 extern 声明

```ebnf
ExternItem         ::= "extern" ExternBody
ExternBody         ::= "type" Ident
                    | "fn" Ident GenericList? "(" ParamList? ")" ("->" Type)?
                    | StringLiteral StringLiteral StringLiteral? ExternForeignDecl
ExternForeignDecl  ::= "type" Ident
                    | Ident "(" ParamList? ")" ("->" Type)?
```

## 4.8 语句与块

```ebnf
Block          ::= "{" BlockElem* "}"
BlockElem      ::= LetStmt | ExprStmt | TailExpr | ";"
LetStmt        ::= "let" Pattern (":" Type)? "=" Expr ";"
ExprStmt       ::= Expr ";"
TailExpr       ::= Expr
```

块的尾表达式规则：

1. 没有 `;` 的最后表达式是尾表达式。
2. 出现尾表达式后，若还出现额外 token（非 `}`），会报错并结束该块解析。

## 4.9 表达式

表达式包括：

1. 字面量、路径。
2. 数组字面量。
3. 元组/括号表达式。
4. 结构体字面量（支持字段简写）。
5. `if`、`match`、`while`、`go`。
6. 闭包 `|...| expr` / `|| expr`。
7. 调用、字段访问、投影（tuple index）、一元/二元运算。

## 4.10 模式

模式包括：

1. 字面量模式：布尔、整数字面量（含显式宽度）、字符串、字符。
2. `_` 通配。
3. 变量模式。
4. tuple/unit 模式。
5. 构造器模式：`Enum::Variant(...)`。
6. 结构体字段模式：`Type { field, field: pat }`。

当前语法不支持浮点字面量模式。

## 4.11 运算符优先级（低到高）

1. `||`
2. `&&`
3. `==`, `!=`
4. `<`, `>`, `<=`, `>=`
5. `+`, `-`
6. `*`, `/`
7. 调用 `(...)`
8. 字段访问 `.` 与前缀 `-`, `!`

---

## 5. 名称解析与可见性

## 5.1 名称解析顺序

单段名称 `x`：

1. 局部变量。
2. 当前包顶层定义。
3. builtin 名称。
4. 未解析名称（诊断）。

多段路径：

1. 先按包/定义路径查找。
2. 若不能作为值路径解析，则可降级为静态成员路径（用于 `Type::method` / `Trait::method`）。

## 5.2 包导入约束

跨包路径需要包已导入（builtin 特例除外）。未导入时报错并继续。

## 5.3 构造器路径规范化

构造器解析支持：

1. `Variant`（若在当前包唯一可确定）。
2. `Enum::Variant`。
3. `Package::Enum::Variant`。

同名 variant 造成歧义时，延后到后续阶段报错。

## 5.4 trait 名称歧义

当未限定 trait 名同时命中本地 trait 与 builtin trait，会报歧义并要求显式包限定。

---

## 6. 类型系统总览

本节是规范核心。

## 6.1 类型宇宙

GoML 当前实现中的核心类型构造如下：

1. 原始类型：`unit`, `bool`, `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64`, `string`, `char`。
2. 组合类型：tuple、array、function。
3. 名义类型：`struct`, `enum`, `extern type`。
4. 泛型相关：`TParam`（类型参数）、`TApp`（类型应用）。
5. 内建容器专用节点：`Ref[T]`, `Vec[T]`, `Slice[T]`, `HashMap[K, V]`。
6. trait 对象：`dyn Trait`。
7. 推断元类型：`TVar`（仅编译期）。

## 6.2 类型比较原则

1. 大多数类型采用结构或名义相等。
2. `struct/enum` 比较使用名称相等。
3. `dyn Trait` 仅同 trait 名相等。
4. `TParam` 仅与同名 `TParam` 统一，不直接与具体类型统一。

---

## 7. 类型构造细则（每个特性逐条说明）

## 7.1 `unit`

定义：唯一值类型，值为 `()`。

静态规则：

1. 无显式返回类型的函数默认返回 `unit`。
2. `while` 与 `go` 表达式类型为 `unit`。
3. `let` 表达式类型为 `unit`。

正例：

```gom
fn noop() {
    ()
}
```

反例：

```gom
fn bad() -> int32 {
    ()
}
```

预期：返回类型不匹配。

## 7.2 布尔类型 `bool`

定义：逻辑布尔。

静态规则：

1. `if` 与 `while` 条件必须约束到 `bool`。
2. `&&`、`||` 两侧必须约束到 `bool`。
3. `!` 一元运算期望 `bool`。

正例：

```gom
fn f(x: bool, y: bool) -> bool {
    !(x && y)
}
```

反例：

```gom
fn f(x: int32) -> bool {
    x && true
}
```

预期：`&&` 操作数非布尔。

## 7.3 整数类型族

定义：`int8/int16/int32/int64/uint8/uint16/uint32/uint64`。

静态规则：

1. 显式后缀整数字面量直接落到对应位宽。
2. 无后缀整数字面量在表达式推断中默认 `int32`。
3. 模式中的无后缀整数优先尝试使用上下文整数类型，否则默认 `int32`。
4. 字面量会做范围校验，越界时报错并以 0 占位继续。

正例：

```gom
fn f() -> unit {
    let a: int8 = 7i8;
    let b = 42;        // 默认 int32
    let _ = a;
    let _ = b;
}
```

反例：

```gom
fn f() -> unit {
    let x: int8 = 300i8;
}
```

预期：字面量越界诊断。

## 7.4 浮点类型族

定义：`float32/float64`。

静态规则：

1. 无后缀浮点字面量默认 `float64`。
2. `f32/f64` 后缀字面量落到对应类型。
3. 解析时会检查有限值与范围（`float32` 范围溢出会诊断）。
4. 浮点字面量解析失败会报错并用 `0.0` 占位。

正例：

```gom
fn g() -> float64 {
    1.25
}
```

反例：

```gom
fn g() -> float32 {
    1e100f32
}
```

预期：超出 `float32` 可表示范围。

## 7.5 字符串 `string`

定义：字符串值。

静态规则：

1. 字符串字面量类型为 `string`。
2. `+` 在当前实现中通过类型等式约束参与推断，常见用法为字符串拼接。

正例：

```gom
fn h() -> string {
    "go" + "ml"
}
```

## 7.6 字符 `char`

定义：单个 Unicode 标量值。

静态规则：

1. 支持常见转义（如 `\n`, `\t`, `\uXXXX`）。
2. 非法字符字面量报错并以 `\0` 占位。

正例：

```gom
fn c() -> char {
    'a'
}
```

反例：

```gom
fn c() -> char {
    '\uZZZZ'
}
```

预期：非法字符字面量。

## 7.7 tuple 类型

定义：有序异构类型集合。

静态规则：

1. tuple 组件逐项类型检查。
2. 投影 `t.0`、`t.1` 仅对 tuple 有效。
3. 非 tuple 投影报错。
4. 投影越界报错。

正例：

```gom
fn t() -> int32 {
    let x: (int32, string) = (7, "ok");
    x.0
}
```

反例：

```gom
fn t() -> int32 {
    let x = (1, 2);
    x.2
}
```

预期：tuple 索引越界。

## 7.8 数组类型 `[T; N]`

定义：固定长度同构数组。

静态规则：

1. 数组字面量通过统一约束所有元素与同一元素类型相等。
2. 推断出的数组类型包含固定长度 `N`。
3. 统一时数组长度必须一致；内部存在 `_` 长度占位可作为通配（实现细节，用于推断/内部约束）。

正例：

```gom
fn arr() -> [int32; 3] {
    [1, 2, 3]
}
```

反例：

```gom
fn arr() -> [int32; 2] {
    [1, 2, 3]
}
```

预期：数组长度不一致。

## 7.9 函数类型 `(A, B) -> C`

定义：显式参数列表与返回类型。

静态规则：

1. 函数调用会创建 `call-site` 函数类型并与被调用项统一。
2. 参数个数必须匹配。
3. 返回类型由统一约束得到。
4. 函数类型在统一时参数数量必须一致。

正例：

```gom
fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn use_add() -> int32 {
    add(1, 2)
}
```

反例：

```gom
fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn bad() -> int32 {
    add(1)
}
```

预期：参数数量不匹配。

## 7.10 闭包类型

定义：闭包也是函数类型值。

静态规则：

1. 闭包参数可注解；无注解时为 fresh TVar。
2. 闭包体类型决定返回类型。
3. 闭包捕获通过局部环境记录：读取外层变量会被标记为捕获。
4. 如果上下文提供期望函数类型且参数个数一致，闭包按期望类型检查。

正例：

```gom
fn mk_adder(k: int32) -> (int32) -> int32 {
    |x| x + k
}
```

反例：

```gom
fn bad() -> (int32) -> int32 {
    |x: string| x
}
```

预期：参数与返回不满足期望函数类型。

## 7.11 名义类型（struct/enum/extern type）

定义：按名称区分的类型。

静态规则：

1. `struct` 与 `enum` 使用名字相等判定。
2. `extern type` 作为名义类型参与类型检查。
3. 泛型类型应用 `Type[A, B]` 会检查类型参数个数。

正例：

```gom
struct Point { x: int32, y: int32 }

fn p() -> Point {
    Point { x: 1, y: 2 }
}
```

反例：

```gom
struct Box[T] { value: T }

fn bad() -> Box {
    Box { value: 1 }
}
```

预期：类型参数个数不匹配。

## 7.12 trait 对象 `dyn Trait`

定义：运行时动态分发对象类型。

静态规则（详见第 12 节）：

1. 仅 dyn-safe trait 可构造 `dyn Trait`。
2. 存在从具体类型/受约束类型到 `dyn Trait` 的类型强制（coercion）。
3. 当前实现中 `dyn` 值的方法调用以 UFCS 形式为主：`Trait::method(x, ...)`。

---

## 8. 类型标注与推断边界

## 8.1 必须显式的地方

1. 顶层函数参数类型必须写出。
2. 顶层函数返回类型可省略，省略时视为 `unit`。
3. `let` 可写可不写类型注解；不写时靠推断。

## 8.2 推断发生位置

1. 表达式节点（`infer_expr`）。
2. 在期望类型上下文中的 `check_expr`（例如函数体、参数位置、闭包目标类型）。
3. pattern 检查中根据被匹配值推断子模式类型。

## 8.3 推断失败

推断失败表现为：

1. 约束无法全部求解。
2. 仍残留未绑定 TVar。
3. 出现 `Could not infer type` 诊断。

---

## 9. 表达式类型规则（逐特性）

## 9.1 变量与名称引用

1. 局部变量返回其局部环境类型。
2. 顶层函数/内建函数引用会先实例化泛型（`inst_ty`）再参与调用。
3. 未解析名称报错并使用 `<error>` 占位表达式继续。

## 9.2 构造器调用与结构体字面量

规则：

1. 构造器参数个数必须匹配。
2. 参数类型按构造器函数类型检查/统一。
3. 结构体字面量字段：
   - 未知字段报错。
   - 重复字段报错。
   - 缺失字段报错，并补占位表达式继续。

正例：

```gom
struct User { id: int32, name: string }

fn u() -> User {
    User { id: 1, name: "a" }
}
```

反例：

```gom
struct User { id: int32, name: string }

fn bad() -> User {
    User { id: 1 }
}
```

预期：缺失 `name` 字段。

## 9.3 调用表达式

调用分流：

1. 调用局部函数值：把变量类型与 `TFunc(args -> ret)` 统一。
2. 调用顶层函数/内建函数：实例化 scheme，检查参数，并应用泛型 trait 约束。
3. 调用 `Type::member` 或 `Trait::method`：走静态成员调用逻辑。
4. 调用 `x.method(...)`：先查 inherent，再查 trait 候选。

## 9.4 一元运算

1. `!e`：约束 `e: bool`，结果 `bool`。
2. `-e`：当前实现对 `-` 在纯推断路径中约束较弱，常见场景依赖上下文期望类型收敛到数值类型。

## 9.5 二元运算

1. `&&`, `||`：两侧约束为 `bool`，结果 `bool`。
2. 比较运算（`==`, `!=`, `<`, `>`, `<=`, `>=`）：两侧类型相等约束，结果 `bool`。
3. `+`, `-`, `*`, `/`：通过等式约束两侧与结果类型相关联；在 check 模式且期望为数值时会直接按该数值类型检查。

## 9.6 字段访问与投影

1. `expr.field`：优先尝试立即解析结构体字段类型。
2. 若无法立即确定，生成 `StructFieldAccess` 约束延后求解。
3. tuple 投影 `expr.i` 仅适用于 tuple。

## 9.7 `if`

规则：

1. 条件约束为 `bool`。
2. then/else 各自推断后统一为同一结果类型。

## 9.8 `match`

规则：

1. 先推断匹配目标类型 `S`。
2. 每个分支 pattern 都按 `S` 检查。
3. 所有分支 body 类型统一到同一结果类型。

注：分支按书写顺序匹配，顺序影响语义。

## 9.9 `while`

规则：

1. 条件必须为 `bool`。
2. 循环体必须约束为 `unit`。
3. 整个 `while` 表达式类型为 `unit`。

## 9.10 `go`

规则：

1. `go expr` 要求 `expr` 的类型是 `() -> unit`。
2. `go` 表达式本身类型为 `unit`。

正例：

```gom
fn main() -> unit {
    let done = ref(false);
    go || {
        ref_set(done, true);
    };
    while !ref_get(done) {}
}
```

---

## 10. 模式类型系统

## 10.1 模式类别与检查

1. `PVar`：把期望类型绑定到变量。
2. `PWild`：生成 fresh TVar 并与期望类型等式约束。
3. 字面量模式：把字面量类型与期望类型约束一致。
4. tuple 模式：逐项检查并与期望 tuple 对齐。
5. 构造器/结构体模式：
   - 先解析构造器。
   - 检查参数/字段数量与类型。
   - 将返回类型与期望类型统一。

## 10.2 `let` 的不可反驳性规则

`let pat = expr` 中 `pat` 必须不可反驳。

当前判定：

1. 允许：变量、`_`、`()`、由这些递归组成的 tuple/struct 模式。
2. 不允许：字面量模式、构造器模式等可反驳模式。

反例：

```gom
enum Option[T] { Some(T), None }

fn f(x: Option[int32]) -> unit {
    let Option::Some(v) = x;
}
```

预期：`let` 使用可反驳模式，建议改 `match`。

## 10.3 结构体模式边界

1. 结构体模式会校验字段数量与名称。
2. 缺失字段会报错并补 `wild` 占位。
3. 多余未知字段会报错。

---

## 11. 泛型系统

## 11.1 泛型实体

当前可见泛型实体：

1. 函数泛型。
2. 结构体/枚举泛型。
3. impl 泛型（含方法级泛型）。

## 11.2 函数方案（FnScheme）

每个函数/方法在环境中记录：

1. `type_params`：类型参数名列表。
2. `constraints`：由 bound 生成的 trait 约束（`T: Trait`）。
3. `ty`：函数类型。

## 11.3 调用点实例化

调用泛型函数时：

1. 先把 `TParam` 替换为 fresh TVar（`inst_ty`）。
2. 再根据调用参数与返回位置生成 `TypeEqual` 约束。
3. 将函数 bound 转化为 `Implements` 或参数 bound 检查。

## 11.4 bound 传播

bound 来源有三类：

1. 顶层函数泛型 bound。
2. impl block 泛型 bound。
3. 方法自身泛型 bound。

方法调用时，会合并并去重这些 bound。

## 11.5 类型参数统一行为（关键边界）

统一器规则：

1. `TParam(name1)` 与 `TParam(name2)` 仅当 `name1 == name2` 才能统一。
2. `TParam` 与具体类型直接统一会报 mismatch。

这意味着：类型参数并不通过统一器直接“改写成具体类型”；它主要在实例化阶段转成 TVar 后参与推断。

---

## 12. Trait 系统

## 12.1 trait 定义

trait 方法在定义处只保存签名，不含方法体。

示例：

```gom
trait Show {
    fn show(Self) -> string;
}
```

## 12.2 trait impl 规则

trait impl 的静态要求：

1. trait 必须存在。
2. orphan rule：trait 或目标类型至少一方为当前包本地。
3. 同一 `(trait, target type)` 不能重复 impl。
4. impl 中的方法名必须都在 trait 声明中。
5. trait 声明的方法必须全部实现。
6. 方法签名必须严格匹配（参数个数、参数类型、返回类型）。

反例（缺失方法）：

```gom
trait EqLike { fn eq(Self, Self) -> bool; }
struct S { v: int32 }

impl EqLike for S {
}
```

预期：缺失 `eq` 方法。

## 12.3 inherent impl 规则

1. 仅允许对本地名义类型定义 inherent impl。
2. 同一 impl 中方法名不可重复。
3. 泛型 inherent impl 以“构造子键”方式注册，非泛型以“精确类型键”注册。

---

## 13. 方法解析

## 13.1 `x.method(...)` 解析顺序

1. 先查 receiver 类型上的 inherent method。
2. 若无，查 trait 方法候选。

trait 候选收集规则：

1. receiver 是 `TParam`：只看该类型参数的 bound trait。
2. receiver 是具体类型：只看“在 scope 内的 trait”且该类型有可见 impl 的候选。

候选结果：

1. 0 个：报 `method not found`。
2. 1 个：选中。
3. 多个：报歧义，要求 UFCS。

## 13.2 UFCS 调用

`Trait::method(x, ...)` 或 `Type::method(...)` 走静态成员调用路径。

优势：

1. 可以显式消歧。
2. 是当前 `dyn Trait` 调用的主要形式。

---

## 14. `dyn Trait` 详细语义

## 14.1 dyn-safe 条件

某 trait 可构造 `dyn Trait`，需要满足：

1. 每个方法必须有接收者参数。
2. 第一个参数必须恰好是 `Self`。
3. 非接收者参数中不能出现 `Self`。
4. 返回类型不能出现 `Self`。

违反任意一条都会报“not dyn-safe”。

## 14.2 向 `dyn Trait` 的 coercion

当某表达式在期望类型上下文中期望 `dyn Trait` 时，coercion 规则：

1. 若表达式已是 `dyn Trait`：不变。
2. 若表达式类型是 `TParam`：该参数必须有对应 trait bound。
3. 若表达式类型是 TVar：生成 `Implements(Trait, TVar)` 约束。
4. 若表达式类型是具体类型：
   - 类型必须“具体化可判定”（不能含未定 TVar / TParam）。
   - 必须存在可见 trait impl。

满足时插入 `ToDyn` coercion。

## 14.3 dyn 调用形式

当前实现特征：

1. `Trait::method(x_dyn, ...)` 可走 dyn 方法路径。
2. `x_dyn.method(...)` 通常不会成功解析（会报 method not found）。

这是当前实现限制，不是语法限制。

---

## 15. 约束系统

## 15.1 约束种类

1. `TypeEqual(l, r)`：类型等式。
2. `Overloaded{ op, trait_name, call_site_type }`：重载方法/trait 选择。
3. `Implements{ trait_name, for_ty }`：trait 实现约束。
4. `StructFieldAccess{ expr_ty, field, result_ty }`：字段访问延迟约束。

## 15.2 约束生成来源

1. `check_expr` 结束时会把表达式类型与期望类型做 `TypeEqual`。
2. 函数/方法调用会产生函数类型等式。
3. trait bound 会转成 `Implements` 或等价检查。
4. 无法立即确定字段类型时生成 `StructFieldAccess`。
5. UFCS trait 调用可能生成 `Overloaded`。

## 15.3 求解流程

求解器按迭代方式运行：

1. 取出所有约束。
2. 对每类约束尝试化简或求解。
3. 若本轮有变化则继续。
4. 无变化但仍有未解决约束则报错退出。

未解决约束会触发：

1. `Could not solve all type constraints`。
2. `Type inference failed due to unresolved constraints`。

---

## 16. 统一（Unification）

## 16.1 TVar 统一

1. `TVar` 与 `TVar`：合并并查集键。
2. `TVar` 与具体类型：occurs-check 通过后绑定。
3. occurs-check 失败报递归类型错误。

## 16.2 结构统一规则

1. tuple：长度必须一致，逐项统一。
2. array：长度必须一致（支持内部 `_` 通配），元素统一。
3. function：参数个数一致，逐项参数与返回统一。
4. `TApp`：构造子与参数个数一致后逐项统一。
5. `Ref/Vec/Slice/HashMap`：递归统一其参数类型。
6. `struct/enum/dyn`：名称相等才可统一。

## 16.3 `TParam` 统一规则

1. `TParam(a)` 与 `TParam(b)`，仅 `a == b` 成功。
2. `TParam` 与非 `TParam` 直接失败并报 mismatch。

---

## 17. 类型检查阶段的环境

## 17.1 局部环境结构

局部环境维护：

1. 作用域栈（局部变量类型）。
2. 当前可见类型参数集合。
3. 类型参数的 trait bound 表。
4. in-scope trait 列表。
5. 闭包捕获栈。

## 17.2 闭包捕获

1. 闭包开始时进入新 capture frame 与新作用域。
2. 访问外层变量时，若跨 scope，会记录到当前闭包 captures。
3. 闭包结束时导出捕获列表。

---

## 18. 内建类型与内建语义

## 18.1 `Ref[T]`

核心操作：`ref`, `ref_get`, `ref_set`。

类型语义：

1. `ref(x): Ref[T]`。
2. `ref_get(r): T`。
3. `ref_set(r, v): unit`。

## 18.2 `Vec[T]`

核心操作：`vec_new`, `vec_push`, `vec_get`, `vec_len`。

当前实现同时支持函数式 API 与方法式 API（通过环境注册）。

## 18.3 `Slice[T]`

核心操作：`slice`, `slice_get`, `slice_len`, `slice_sub`。

语义：只读视图类型。

## 18.4 `HashMap[K, V]`

核心操作：`hashmap_new`, `hashmap_get`, `hashmap_set`, `hashmap_remove`, `hashmap_len`, `hashmap_contains`。

键语义依赖 `Eq` 与 `Hash`。

## 18.5 关键边界：`Ref` 与 `Eq/Hash`

当前实现中的 builtin 语义是：

1. `Ref[T]` 的 `Eq/Hash` 以内容为依据（不是默认指针身份）。
2. 若需要指针身份比较，使用专门 builtin（如 `ptr_eq`）。

---

## 19. 控制结构与类型语义

## 19.1 `let`

1. 带注解时：右值按注解类型检查。
2. 不带注解时：右值先推断。
3. pattern 检查后将绑定写入当前作用域。
4. `let` 自身类型为 `unit`。

## 19.2 block

1. 空 block 类型是 `unit`。
2. 非空 block 类型是最后表达式类型。
3. 在 check 模式下，最后表达式按期望类型检查。

## 19.3 `if`

1. 条件 `bool`。
2. 两分支统一同一结果类型。

## 19.4 `match`

1. 每个分支有独立局部作用域。
2. 分支 pattern 可引入绑定。
3. 分支 body 类型统一。

## 19.5 `while`

1. 条件 `bool`。
2. body 约束为 `unit`。
3. 整体为 `unit`。

## 19.6 `go`

1. 参数表达式必须是 `() -> unit`。
2. 整体返回 `unit`。

---

## 20. 错误模型与恢复

## 20.1 总体原则

1. 尽可能 recoverable：报错后继续构建后续 IR。
2. 名称解析、类型检查、统一阶段都偏向“收集更多错误”。

## 20.2 常见诊断分类

1. 语法：缺 token、顺序违规、移除语法使用。
2. 名称：未解析标识符、包未导入、构造器歧义。
3. 类型：mismatch、参数个数不符、方法找不到、impl 违规。
4. 推断：未解约束、未推断类型变量。
5. dyn：unknown trait、not dyn-safe、无法转换到 dyn。

## 20.3 占位策略

发生错误时常见占位行为：

1. 表达式替换为 `<error>` 变量并赋 fresh TVar。
2. 缺字段/缺模式参数时补占位，继续后续检查。
3. 字面量解析失败时使用零值占位继续。

---

## 21. 当前实现边界与易错点

## 21.1 已知边界

1. `return` 关键字暂无语句语法。
2. `import` 语法已移除。
3. `dyn` 方法调用当前以 UFCS 为主，`x_dyn.method()` 通常不工作。
4. 浮点字面量模式当前不支持。
5. `let` 不支持可反驳模式。

## 21.2 推断相关边界

1. `TParam` 与具体类型不直接统一，错误往往表现为类型不匹配而非“自动实例化”。
2. 某些一元/算术路径的数值性约束依赖上下文，可能表现为“延后报错”。

## 21.3 方法解析相关边界

1. 同名 trait 方法冲突时必须用 UFCS 显式指定。
2. 在 concrete receiver 上，trait 方法要求 trait 在 scope 且 impl 可见。
3. 在 type parameter receiver 上，trait 方法只看该类型参数 bound。

---

## 22. 类型系统示例库（按特性）

## 22.1 泛型函数 + bound

```gom
trait Show {
    fn show(Self) -> string;
}

fn render[T: Show](x: T) -> string {
    Show::show(x)
}
```

## 22.2 trait impl + 签名一致性

```gom
trait AddOne {
    fn apply(Self) -> int32;
}

struct Box { v: int32 }

impl AddOne for Box {
    fn apply(self: Box) -> int32 {
        self.v + 1
    }
}
```

## 22.3 inherent 方法与 trait 方法同名

```gom
trait Render {
    fn format(Self) -> string;
}

struct Boxed { v: int32 }

impl Boxed {
    fn format(self: Boxed) -> string {
        "inherent"
    }
}

impl Render for Boxed {
    fn format(self: Boxed) -> string {
        int32_to_string(self.v)
    }
}

fn main() -> unit {
    let b = Boxed { v: 9 };
    let _ = Boxed::format(b);
    let _ = Render::format(b);
}
```

## 22.4 `dyn` 转换与 UFCS 调用

```gom
trait Display {
    fn to_string(Self) -> string;
}

struct Point { x: int32, y: int32 }

impl Display for Point {
    fn to_string(self: Point) -> string {
        "(" + int32_to_string(self.x) + "," + int32_to_string(self.y) + ")"
    }
}

fn show(x: dyn Display) -> string {
    Display::to_string(x)
}

fn main() -> unit {
    let p = Point { x: 1, y: 2 };
    let d: dyn Display = p;
    let _ = show(d);
}
```

## 22.5 闭包捕获

```gom
fn make_counter() -> () -> int32 {
    let n = ref(0);
    || {
        let v = ref_get(n) + 1;
        ref_set(n, v);
        v
    }
}
```

## 22.6 `let` 可反驳模式反例

```gom
enum Option[T] { Some(T), None }

fn bad(x: Option[int32]) -> unit {
    let Option::Some(v) = x;
}
```

预期：报错，建议使用 `match`。

## 22.7 struct 字段缺失反例

```gom
struct User { id: int32, name: string }

fn bad() -> User {
    User { id: 1 }
}
```

预期：缺失字段 `name`。

## 22.8 泛型参数约束不足反例

```gom
trait Display {
    fn to_string(Self) -> string;
}

fn bad[T](x: T) -> string {
    Display::to_string(x)
}
```

预期：`T` 未被 `Display` 约束。

## 22.9 UFCS 消歧

```gom
trait A { fn pick(Self) -> int32; }
trait B { fn pick(Self) -> int32; }

struct S { v: int32 }

impl A for S { fn pick(self: S) -> int32 { self.v } }
impl B for S { fn pick(self: S) -> int32 { self.v + 10 } }

fn f(x: S) -> int32 {
    A::pick(x)
}
```

## 22.10 `go` 类型要求

```gom
fn main() -> unit {
    go || {
        let _ = string_println("worker");
        ()
    };
}
```

---

## 23. 类型系统速查表

1. `let` 绑定：模式必须不可反驳。
2. 泛型调用：先实例化（`TParam -> TVar`），后加约束。
3. `TParam` 不直接与具体类型统一。
4. trait 方法解析：inherent 优先，trait 次之；歧义用 UFCS。
5. `dyn Trait`：必须 dyn-safe；调用优先 UFCS。
6. `go`：参数表达式类型必须是 `() -> unit`。
7. 结构体字段访问可延迟求解（`StructFieldAccess`）。
8. 约束未解会产生“无法求解约束/无法推断类型”诊断。

---

## 24. 规范性结论

1. GoML 当前类型系统是“约束驱动 + 统一求解 + recoverable diagnostics”的实现。
2. trait/impl/dyn 的规则已经成体系，但 `dyn` 点调用和部分推断边界仍保留实现约束。
3. 编写代码时，应优先：
   - 在泛型处明确 bound。
   - 在方法冲突处使用 UFCS。
   - 在 `dyn` 场景使用 `Trait::method(x, ...)`。
   - 在 `let` 绑定使用不可反驳模式。

本规范即当前编译器行为的语义基线。

---

## 25. 按语法结构细化：Trait、约束生成、约束求解

本章给出实现级细化，直接对应 typer 中的表达式/模式检查分支。

统一前提：

1. 任何表达式进入 `check_expr(e, expected)` 后，都会在末尾追加 `TypeEqual(type(e), expected)`。
2. `check_expr` 末尾会尝试 `coerce_to_expected_dyn`，即当 `expected` 是 `dyn Trait` 时先做 dyn 强制，再做类型等式。
3. 因此，下面每个结构都分为“推断路径（infer）”和“检查路径（check）”两种语义。

### 25.1 约束类型速记

1. `TEQ(a, b)` = `TypeEqual(a, b)`。
2. `OVL(op, trait, call_ty)` = `Overloaded`。
3. `IMP(trait, ty)` = `Implements`。
4. `SFA(expr_ty, field, result_ty)` = `StructFieldAccess`。

---

## 26. 顶层语法结构（声明层）

### 26.1 `trait` 定义

Trait 参与：

1. 直接向 trait 环境注册方法签名（`FnScheme`，无函数体）。

约束生成：

1. 声明阶段不生成运行时表达式约束。
2. 仅在后续调用或 impl 校验时转化为约束/比较。

求解路径：

1. 无直接 solver 输入。
2. 影响 `resolve_trait_name`、方法查找与 `OVL/IMP` 的合法性。

失败诊断：

1. 若 trait 名无法解析，后续使用点报 `Unknown trait`。

### 26.2 `impl Trait for Type`

Trait 参与：

1. 校验 orphan rule。
2. 校验方法完整性与签名严格一致性。

约束生成：

1. impl 注册时不直接推送 `Constraint`。
2. 方法体类型检查时，方法调用和表达式会生成普通约束。

求解路径：

1. 由方法体中的约束进入 `solve`。
2. solver 在 `OVL/IMP` 查询中可见该 impl。

失败诊断：

1. trait 不存在。
2. orphan rule 违规。
3. 重复 impl。
4. 缺失方法。
5. 额外方法。
6. 参数个数/参数类型/返回类型与 trait 签名不一致。

### 26.3 `impl Type`（inherent impl）

Trait 参与：

1. 不涉及 trait 实现表，但会写入 inherent 方法表。

约束生成：

1. 注册阶段无直接 `Constraint`。
2. 方法体检查中会生成常规约束。

求解路径：

1. 调用点优先命中 inherent 方法，再进入函数类型统一。

失败诊断：

1. 对非本地类型定义 inherent impl。
2. 同一 impl 中方法重复。

### 26.4 顶层函数 `fn`

Trait 参与：

1. 泛型 bound 进入 `FnScheme.constraints`。

约束生成：

1. 函数体 `check_expr(body, ret_ty)` 生成表达式约束。
2. 调用点才会将 `FnScheme.constraints` 转成 `IMP` 或 bound 检查。

求解路径：

1. 每个函数体类型检查后都会运行 `solve`。

失败诊断：

1. 返回类型不匹配。
2. 泛型参数/约束使用错误会在类型验证或调用点报错。

### 26.5 `extern` / `extern builtin`

Trait 参与：

1. `extern builtin` 可携带泛型 bound，进入 `FnScheme.constraints`。

约束生成：

1. 声明时不生成求解约束。
2. 调用时按普通函数调用生成 `TEQ/IMP`。

求解路径：

1. 与普通函数调用一致。

失败诊断：

1. 类型构造器未知、泛型参数个数错误、非法类型参数等。

---

## 27. 表达式结构逐项细化

### 27.1 `ENameRef`

Trait 参与：

1. 无直接 trait 分派。
2. 若引用的是函数值，后续调用时才触发 trait bound 约束。

约束生成：

1. `infer` 阶段仅产出类型（局部变量类型或实例化后的函数类型）。
2. `check` 阶段统一追加 `TEQ(type(expr), expected)`。

求解路径：

1. 仅通过外层 `TEQ` 参与统一。

失败诊断：

1. 未解析名称：`Unresolved name ...`。

### 27.2 `EStaticMember`（非调用）

Trait 参与：

1. 可能解析成 `ETraitMethod` 或 `EInherentMethod` 的函数值。

约束生成：

1. 仅产出方法函数类型，不立即生成 `OVL`。
2. 在 `check` 路径上会追加 `TEQ`。

求解路径：

1. 后续若被调用，才进入对应调用分支求解。

失败诊断：

1. 类型或 trait 不存在。
2. 方法不存在。

### 27.3 `EUnit`

Trait 参与：无。

约束生成：

1. `infer` 得到 `unit`。
2. `check` 追加 `TEQ(unit, expected)`。

求解路径：仅统一。

失败诊断：通常是外层类型不匹配。

### 27.4 `EBool`

Trait 参与：无。

约束生成：

1. `infer` 得到 `bool`。
2. `check` 追加 `TEQ(bool, expected)`。

求解路径：仅统一。

### 27.5 `EInt/EInt8/EInt16/EInt32/EInt64/EUInt8/EUInt16/EUInt32/EUInt64`

Trait 参与：无直接 trait。

约束生成：

1. `infer` 直接给定对应整数类型（无后缀 `int` 默认为 `int32`）。
2. `check` 追加 `TEQ(lit_ty, expected)`。

求解路径：

1. 普通统一。

失败诊断：

1. 字面量解析失败。
2. 越界（不适配目标位宽）。

### 27.6 `EFloat/EFloat32/EFloat64`

Trait 参与：无直接 trait。

约束生成：

1. `infer` 给定浮点类型（无后缀默认 `float64`）。
2. `check` 追加 `TEQ(lit_ty, expected)`。

求解路径：普通统一。

失败诊断：

1. 非法浮点字面量。
2. `float32` 范围不适配。

### 27.7 `EString`

Trait 参与：无直接 trait。

约束生成：

1. `infer` 为 `string`。
2. `check` 追加 `TEQ(string, expected)`。

求解路径：普通统一。

### 27.8 `EChar`

Trait 参与：无直接 trait。

约束生成：

1. `infer` 为 `char`。
2. `check` 追加 `TEQ(char, expected)`。

求解路径：普通统一。

失败诊断：

1. 非法字符字面量。

### 27.9 `EConstr`（构造器表达式）

Trait 参与：

1. 无直接 trait 方法分派。

约束生成：

1. 校验构造器参数个数。
2. 构造器类型实例化后：
   - 若有参数，生成 `TEQ(inst_constr_ty, (arg_types) -> ret_ty)`。
   - 若无参数，生成 `TEQ(inst_constr_ty, ret_ty)`。
3. 各参数按构造器参数类型进入 `check_expr`，内部再生成约束。

求解路径：

1. 由 `TEQ` 统一构造器参数与返回类型。

失败诊断：

1. 构造器不存在。
2. 构造器歧义。
3. 参数个数不匹配。

### 27.10 `EStructLiteral`

Trait 参与：无直接 trait 分派。

约束生成：

1. 字段名映射并做重复/未知检查。
2. 每个字段按构造器参数预期类型 `check_expr`。
3. 最终生成与 `EConstr` 同形态 `TEQ`。
4. 缺失字段会补占位表达式，仍参与约束。

求解路径：

1. 与 `EConstr` 一致。

失败诊断：

1. 目标不是结构体构造器。
2. 未知字段、重复字段、缺失字段。

### 27.11 `ETuple`

Trait 参与：无。

约束生成：

1. `infer` 对每个元素递归推断，不直接生成额外 `Constraint`。
2. `check` 场景若 `expected` 为同长度 tuple，会逐项 `check_expr`；最终仍有外层 `TEQ`。

求解路径：

1. 外层统一时逐项统一。

### 27.12 `EArray`

Trait 参与：无。

约束生成：

1. 生成 fresh `elem_ty`。
2. 每个元素推断后生成 `TEQ(item_ty, elem_ty)`。
3. 数组表达式类型为 `[elem_ty; len]`。

求解路径：

1. 通过 `TEQ` 将元素类型收敛。

失败诊断：

1. 元素类型不一致导致统一失败。

### 27.13 `EClosure`

Trait 参与：

1. 无直接 trait 分派。
2. 但闭包可在 expected=`dyn Trait` 时触发 dyn coercion（如果闭包类型可满足）。

约束生成：

1. `infer`：参数无注解时 fresh TVar；体推断后形成函数类型。
2. `check`（expected 为函数类型且参数个数匹配）：
   - 参数注解存在时生成 `TEQ(annotation_ty, expected_param_ty)`。
   - 体按 expected return 检查。
3. `check` 末尾统一追加 `TEQ`。

求解路径：

1. 参数与返回的等式约束统一。

失败诊断：

1. 参数个数/类型与 expected 函数类型不匹配。

### 27.14 `ELet`

Trait 参与：

1. 无直接 trait 分派。

约束生成：

1. 有注解：`value` 走 `check_expr(value, ann_ty)`。
2. 无注解：`value` 走 `infer_expr`。
3. `check_pat(pat, value_ty)` 会生成 pattern 相关约束。
4. `let` 自身类型固定 `unit`。

求解路径：

1. 来自 value 与 pattern 的约束进入 solver。

失败诊断：

1. 可反驳模式用于 `let`。

### 27.15 `EBlock`

Trait 参与：无直接 trait。

约束生成：

1. `infer`：逐个推断，最后一个表达式类型为 block 类型。
2. `check`：最后一个表达式按 `expected` 检查。
3. `check` 末尾追加 `TEQ(block_ty, expected)`。

求解路径：由子表达式约束驱动。

### 27.16 `EMatch`

Trait 参与：无直接 trait 分派。

约束生成：

1. 推断 scrutinee 类型 `S`。
2. 每个 arm pattern 执行 `check_pat(..., S)`。
3. `infer` 模式：每个 arm body 生成 `TEQ(arm_body_ty, arm_result_ty)`（共享 fresh）。
4. `check` 模式：每个 arm body 直接 `check_expr(..., expected)`。

求解路径：

1. 统一 arm body 类型。
2. pattern 约束统一 scrutinee 与各模式子类型。

失败诊断：

1. 构造器模式错误。
2. 模式参数个数错误。
3. 结构体模式字段错误。

### 27.17 `EIf`

Trait 参与：无。

约束生成：

1. 条件生成 `TEQ(cond_ty, bool)`。
2. `infer` 模式：then/else 各自与 fresh result_ty 生成 `TEQ`。
3. `check` 模式：then/else 直接按 `expected` 检查，末尾再 `TEQ`。

求解路径：统一分支结果类型。

### 27.18 `EWhile`

Trait 参与：无。

约束生成：

1. `TEQ(cond_ty, bool)`。
2. `TEQ(body_ty, unit)`。
3. 表达式类型 `unit`，`check` 场景再 `TEQ(unit, expected)`。

求解路径：普通统一。

### 27.19 `EGo`

Trait 参与：无。

约束生成：

1. 生成 `TEQ(expr_ty, () -> unit)`。
2. 表达式类型 `unit`，`check` 场景追加 `TEQ(unit, expected)`。

求解路径：普通统一。

### 27.20 `ECall`（总入口）

`ECall` 是 trait 与约束最密集节点，需按 callee 形态分流。

#### 27.20.1 调用局部变量函数值

Trait 参与：

1. 无直接 trait 查找。

约束生成：

1. 先推断所有参数，构造 `call_site_ty = (arg_tys) -> ret_ty`。
2. 生成 `TEQ(var_ty, call_site_ty)`。

求解路径：

1. 统一变量函数类型与调用点函数类型。

#### 27.20.2 调用顶层函数/内建函数

Trait 参与：

1. 读取 `FnScheme.constraints`，每条 bound 通过 `apply_fn_scheme_constraints` 处理。

约束生成：

1. 函数类型实例化 `inst_ty = inst_ty(scheme.ty)`。
2. 参数数量匹配且有参数时，按参数类型 `check_expr`；否则先 `infer` 参数。
3. 构造 `call_site_ty = (arg_tys) -> ret_ty`。
4. 生成 `TEQ(inst_ty, call_site_ty)`。
5. 对 bound：
   - 实参对应类型为 `dyn Trait` 且同 trait：直接满足。
   - 为 `TParam`：要求 local bound 含该 trait，否则报错。
   - 其余类型：生成 `IMP(trait, actual_ty)`。

求解路径：

1. `TEQ` 统一参数/返回。
2. `IMP` 在 solver 中检查 impl 可见性或 dyn 满足性。

#### 27.20.3 单段未解析名称的函数回退调用

Trait 参与与约束生成：与 27.20.2 相同（通过 unqualified 函数查找）。

#### 27.20.4 `TypeOrTrait::member(...)`（静态成员调用）

分两类：

1. 解析为 trait 方法。
2. 解析为 inherent 方法。

##### A. 解析为 trait 方法

Trait 参与：

1. 通过 type name 识别 trait。
2. receiver 在参数第 1 位。

约束生成：

1. 先实例化方法类型，再用 receiver 进行 `Self` 替换。
2. 若第一个实参是 `dyn Trait` 且 trait 匹配：
   - 构造 dyn 方法调用，不生成 `OVL`，按参数 `check_expr`。
3. 若 receiver 是 `TParam`：
   - 检查其 bound 是否包含 trait；满足时生成 `TEQ(call_site_ty, method_ty_after_self)`。
4. 若 receiver 是非 `TParam`：
   - 生成 `OVL(method_name, trait_name, call_site_ty)`。

求解路径：

1. `TParam` 路径走 `TEQ` 直接统一。
2. `OVL` 路径在 solver 中：
   - receiver concrete 时收集可见 impl。
   - 1 个 impl -> 转化为 `TEQ(call_site_ty, impl_fun_ty)`。
   - 0 或多 -> 报错。
   - receiver 非 concrete -> 延期。

##### B. 解析为 inherent 方法

Trait 参与：

1. 不走 trait 表。

约束生成：

1. 实例化方法类型，检查参数个数。
2. 每个参数 `check_expr`。
3. 构造 `call_site_ty` 并生成 `TEQ(inst_method_ty, call_site_ty)`。
4. 应用方法 scheme 的泛型 bound，必要时生成 `IMP`。

求解路径：

1. `TEQ` + `IMP`。

#### 27.20.5 `x.method(...)`（字段式方法调用）

Trait 参与：

1. 先查 inherent。
2. 未命中再查 trait 候选。

约束生成：

1. inherent 命中：
   - 构造包含 receiver 的 `call_site_ty`。
   - 生成 `TEQ(inst_method_ty, call_site_ty)`。
   - 应用方法 bound，必要时生成 `IMP`。
2. trait 命中且唯一：
   - 方法类型实例化并以 receiver 替换 `Self`。
   - 参数按 expected 检查。
   - 生成 `TEQ(receiver_ty, receiver_param_ty)`。
3. 无候选或多候选：不生成有效调用约束，直接错误占位。

求解路径：

1. inherent 路径：`TEQ/IMP`。
2. trait 唯一路径：receiver 等式统一 + 子参数约束。

失败诊断：

1. method not found。
2. ambiguous method（提示 UFCS）。

### 27.21 `EUnary`

Trait 参与：无。

约束生成：

1. `!`：`TEQ(expr_ty, bool)`。
2. `-`：当前实现仅生成 `TEQ(expr_ty, expr_ty)`（形式上恒真），数值性主要依赖上下文。

求解路径：

1. `!` 通过普通统一收敛到 `bool`。
2. `-` 的数值约束更多来自外部 `check` 期望或后续运算约束。

### 27.22 `EBinary`

Trait 参与：无直接 trait 分派。

约束生成：

1. `+/-/*//`：
   - `ret_ty` 在 infer 下 fresh。
   - 生成 `TEQ(lhs_ty, ret_ty)` 与 `TEQ(rhs_ty, ret_ty)`。
2. `&&/||`：`TEQ(lhs_ty, bool)` 与 `TEQ(rhs_ty, bool)`，结果 `bool`。
3. 比较运算：`TEQ(lhs_ty, rhs_ty)`，结果 `bool`。

求解路径：普通统一。

### 27.23 `EProj`

Trait 参与：无。

约束生成：

1. tuple 正常时直接返回投影类型，不新增约束。
2. 非 tuple 或越界报错并返回 fresh TVar 占位。

求解路径：通过外层约束继续。

### 27.24 `EField`

Trait 参与：无直接 trait。

约束生成：

1. 若能立即解析结构体字段类型，直接返回该类型。
2. 否则生成 `SFA(base_ty, field, result_ty)`。

求解路径：

1. `SFA` 在 solver 中：
   - 当 `expr_ty` 可分解为 struct 类型后，实例化字段类型。
   - 再统一 `result_ty` 与实例化字段类型。
   - 不能分解时延期。

失败诊断：

1. 结构体不存在。
2. 字段不存在。

### 27.25 `EToDyn`（由 coercion 产生）

Trait 参与：

1. 核心是 `dyn` 强制。

约束生成：

1. 若源类型是 `TVar`，生成 `IMP(trait, for_ty)`。
2. 若源类型是 `TParam`，检查 bound，不生成 `IMP`。
3. 若源类型是 concrete，检查 impl 可见性，不生成 `IMP`。
4. 结果类型固定为 `dyn Trait`，随后还有外层 `TEQ`。

求解路径：

1. `IMP` 在 solver 验证 impl。

失败诊断：

1. `TParam` 无对应 bound。
2. 源类型非 concrete 且不是 `TVar` 可延期情形。
3. concrete 类型无 trait impl。

---

## 28. 模式结构逐项细化

### 28.1 `PVar`

Trait 参与：无。

约束生成：

1. 不生成 `Constraint`。
2. 直接把期望类型绑定到变量。

求解路径：

1. 无新增约束，依赖上下文表达式约束。

### 28.2 `PWild`

Trait 参与：无。

约束生成：

1. 生成 fresh `pat_ty`。
2. 生成 `TEQ(pat_ty, expected_ty)`。

求解路径：普通统一。

### 28.3 `PUnit`

Trait 参与：无。

约束生成：

1. 当前实现直接返回 `unit` 模式类型。
2. 不额外推送 `TEQ(unit, expected)`。

求解路径：

1. 主要依赖外层其它约束。

边界说明：

1. 这是当前实现细节，与“严格检查 unit 模式类型”直觉可能不同。

### 28.4 `PBool`

Trait 参与：无。

约束生成：

1. 当前实现直接返回 `bool` 模式类型。
2. 不额外推送 `TEQ(bool, expected)`。

求解路径：主要依赖外层。

边界说明：

1. 同 28.3，为当前实现细节。

### 28.5 `PInt`（无后缀）

Trait 参与：无。

约束生成：

1. 若 expected 是整数类型，则字面量按该整数类型解析。
2. 否则按 `int32` 解析。
3. 生成 `TEQ(target_int_ty, expected_ty)`。

求解路径：普通统一。

### 28.6 `PInt8/PInt16/PInt32/PInt64/PUInt8/PUInt16/PUInt32/PUInt64`

Trait 参与：无。

约束生成：

1. 字面量按固定类型解析。
2. 生成 `TEQ(literal_ty, expected_ty)`。

求解路径：普通统一。

### 28.7 `PString`

Trait 参与：无。

约束生成：

1. 生成 `TEQ(string, expected_ty)`。

### 28.8 `PChar`

Trait 参与：无。

约束生成：

1. 生成 `TEQ(char, expected_ty)`。

### 28.9 `PTuple`

Trait 参与：无。

约束生成：

1. 对每个子模式递归 `check_pat`。
2. 形成 `pat_tuple_ty`。
3. 生成 `TEQ(pat_tuple_ty, expected_ty)`。

求解路径：普通统一。

### 28.10 `PConstr`（构造器模式）

Trait 参与：无直接 trait 分派。

约束生成：

1. 构造器解析并实例化类型。
2. 子模式按构造器参数类型检查。
3. 生成 `TEQ(constructor_ret_ty, expected_ty)`。

求解路径：

1. 统一构造器返回类型与匹配目标类型。

失败诊断：

1. 构造器歧义。
2. 构造器不存在。
3. 参数个数错误。
4. 结构体构造器误用为位置参数模式。

### 28.11 `PStruct`（结构体字段模式）

Trait 参与：无。

约束生成：

1. 校验结构体定义与字段集合。
2. 每个字段子模式按对应字段类型检查。
3. 缺失字段补 `wild` 并生成其约束。
4. 生成 `TEQ(struct_ret_ty, expected_ty)`。

求解路径：普通统一。

失败诊断：

1. 结构体不存在。
2. 字段缺失/重复/未知。

---

## 29. 约束求解的状态机细化

本节把 solver 行为写成“可求解/延期/失败”三态。

### 29.1 `TEQ` 状态机

1. 可求解：
   - 两侧结构可统一。
2. 延期：
   - 不延期，`TEQ` 直接尝试统一。
3. 失败：
   - 结构不兼容、arity 不匹配、`TParam` 与具体类型冲突、occurs-check 失败。

### 29.2 `OVL` 状态机

1. 可求解：
   - `call_site_type` 归一化后是函数类型，且 receiver 已 concrete。
   - 可见 impl 数量恰好为 1。
   - 转化为 `TEQ(call_fun_ty, impl_fun_ty)`。
2. 延期：
   - receiver 仍是 `TVar` 或非 concrete。
3. 失败：
   - 无 impl。
   - 多个 impl。
   - call_site_type 非函数类型。

### 29.3 `IMP` 状态机

1. 可求解：
   - `for_ty` 是 `dyn Trait` 且同 trait；或
   - 可见 impl 存在。
2. 延期：
   - `for_ty` 非 concrete（如含 TVar）。
3. 失败：
   - 类型 concrete 且找不到 impl。

### 29.4 `SFA` 状态机

1. 可求解：
   - `expr_ty` 可分解到 struct 构造子。
   - 字段存在并可实例化字段类型。
   - 统一 `result_ty` 与字段类型。
2. 延期：
   - `expr_ty` 当前无法分解（尚不 concrete）。
3. 失败：
   - struct 不存在或字段不存在。

### 29.5 求解停止条件

1. 一轮迭代中无任何变化，且仍有未决约束 -> 报 `Could not solve all type constraints`。
2. 退出后仍有残余约束 -> 报 `Type inference failed due to unresolved constraints`。
3. 替换阶段遇到未绑定 TVar -> 报 `Could not infer type`。

---

## 30. 语法结构 -> 约束映射清单（完整）

### 30.1 表达式节点

1. `ENameRef`: `TEQ`（仅 check 末尾）。
2. `EStaticMember`: `TEQ`（仅 check 末尾）。
3. `EUnit`: `TEQ(unit, expected)`（check）。
4. `EBool`: `TEQ(bool, expected)`（check）。
5. `EInt* / EUInt*`: `TEQ(lit_ty, expected)`（check）。
6. `EFloat*`: `TEQ(lit_ty, expected)`（check）。
7. `EString`: `TEQ(string, expected)`（check）。
8. `EChar`: `TEQ(char, expected)`（check）。
9. `EConstr`: `TEQ(inst_constr_ty, call_like_ty)`。
10. `EStructLiteral`: `TEQ(inst_constr_ty, call_like_ty)`。
11. `ETuple`: 无特定约束（子表达式产生）。
12. `EArray`: 每元素 `TEQ(item_ty, elem_ty)`。
13. `EClosure`: 注解参数与 expected 参数的 `TEQ`（check closure）。
14. `ELet`: 来自 value + pattern 的约束；`ELet` 自身无新约束。
15. `EBlock`: 无特定约束（子表达式产生）。
16. `EMatch`: 每 arm `TEQ(arm_body_ty, arm_result_ty)`（infer）。
17. `EIf`: `TEQ(cond_ty, bool)` + 两分支到同一结果类型。
18. `EWhile`: `TEQ(cond_ty, bool)` + `TEQ(body_ty, unit)`。
19. `EGo`: `TEQ(expr_ty, () -> unit)`。
20. `ECall(local fn value)`: `TEQ(var_ty, call_site_ty)`。
21. `ECall(top-level/builtin)`: `TEQ(inst_fn_ty, call_site_ty)` + bound 导出的 `IMP`。
22. `ECall(static trait)`: `OVL` 或 `TEQ`（TParam 路径）或 dyn 特化。
23. `ECall(static inherent)`: `TEQ(inst_method_ty, call_site_ty)` + 可能 `IMP`。
24. `ECall(field inherent)`: `TEQ(inst_method_ty, call_site_ty)` + 可能 `IMP`。
25. `ECall(field trait unique)`: `TEQ(receiver_ty, receiver_param_ty)`。
26. `EUnary(!)`: `TEQ(expr_ty, bool)`。
27. `EUnary(-)`: 仅自等式（实现细节）。
28. `EBinary(add/sub/mul/div)`: `TEQ(lhs, ret)` + `TEQ(rhs, ret)`。
29. `EBinary(logical)`: `TEQ(lhs, bool)` + `TEQ(rhs, bool)`。
30. `EBinary(compare)`: `TEQ(lhs, rhs)`。
31. `EProj`: 无新约束。
32. `EField`: `SFA(...)`（延迟场景）。
33. `EToDyn`: 可能 `IMP(trait, for_ty)`（当 `for_ty` 是 TVar）。

### 30.2 模式节点

1. `PVar`: 无新约束。
2. `PWild`: `TEQ(fresh, expected)`。
3. `PUnit`: 当前实现无显式 `TEQ(unit, expected)`。
4. `PBool`: 当前实现无显式 `TEQ(bool, expected)`。
5. `PInt`: `TEQ(target_int_ty, expected)`。
6. `PInt*/PUInt*`: `TEQ(lit_ty, expected)`。
7. `PString`: `TEQ(string, expected)`。
8. `PChar`: `TEQ(char, expected)`。
9. `PTuple`: `TEQ(tuple_pat_ty, expected)`。
10. `PConstr`: `TEQ(constr_ret_ty, expected)`。
11. `PStruct`: `TEQ(struct_ret_ty, expected)`。

---

## 31. 规范性补充结论（针对 trait 与约束系统）

1. GoML 的 trait 语义不是“语法糖分派”，而是“方法查找 + 约束生成 + 统一求解”的组合系统。
2. trait 相关约束来源集中在三处：
   - 泛型 bound（`FnScheme.constraints` -> `IMP`）。
   - 静态 trait 调用（`OVL` 或 receiver `TEQ`）。
   - dyn coercion（`IMP` 或 bound 检查）。
3. 求解器是渐进式：优先求解 concrete 情况，非 concrete 延期，最终残留即报错。
4. 逐语法结构细化后，所有表达式与模式节点都具备明确的“约束生成/求解/失败”定义。
