package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Expr interface {
    isExpr()
}

type Zero struct {}

func (_ Zero) isExpr() {}

type Succ struct {
    _0 Expr
}

func (_ Succ) isExpr() {}

type Add struct {
    _0 Expr
    _1 Expr
}

func (_ Add) isExpr() {}

type Mul struct {
    _0 Expr
    _1 Expr
}

func (_ Mul) isExpr() {}

func main0() struct{} {
    var t50 Expr
    var a__0 Expr
    var x1 Expr
    var x2 Expr
    var x10 Expr
    var x15 Expr
    var x20 Expr
    var x25 Expr
    var x3 Expr
    var x4 Expr
    var x31 Expr
    var x32 Expr
    t50 = Add{
        _0: Zero{},
        _1: Zero{},
    }
    a__0 = Mul{
        _0: t50,
        _1: Zero{},
    }
    switch a__0.(type) {
    case Zero:
        goto b2
    case Succ:
        goto b3
    case Add:
        goto b4
    case Mul:
        goto b30
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    print__T_int32(6)
    goto b1
    b3:
    print__T_int32(6)
    goto b1
    b4:
    x1 = a__0.(Add)._0
    x2 = a__0.(Add)._1
    switch x2.(type) {
    case Zero:
        goto b6
    case Succ:
        goto b12
    case Add:
        goto b18
    case Mul:
        goto b24
    default:
        panic("non-exhaustive match")
    }
    b5:
    goto b1
    b6:
    switch x1.(type) {
    case Zero:
        goto b8
    case Succ:
        goto b9
    case Add:
        goto b10
    case Mul:
        goto b11
    default:
        panic("non-exhaustive match")
    }
    b7:
    goto b5
    b8:
    print__T_int32(0)
    goto b7
    b9:
    x10 = x1.(Succ)._0
    print__T_int32(2)
    goto b7
    b10:
    print__T_int32(5)
    goto b7
    b11:
    print__T_int32(5)
    goto b7
    b12:
    switch x1.(type) {
    case Zero:
        goto b14
    case Succ:
        goto b15
    case Add:
        goto b16
    case Mul:
        goto b17
    default:
        panic("non-exhaustive match")
    }
    b13:
    goto b5
    b14:
    print__T_int32(6)
    goto b13
    b15:
    x15 = x1.(Succ)._0
    print__T_int32(2)
    goto b13
    b16:
    print__T_int32(6)
    goto b13
    b17:
    print__T_int32(6)
    goto b13
    b18:
    switch x1.(type) {
    case Zero:
        goto b20
    case Succ:
        goto b21
    case Add:
        goto b22
    case Mul:
        goto b23
    default:
        panic("non-exhaustive match")
    }
    b19:
    goto b5
    b20:
    print__T_int32(6)
    goto b19
    b21:
    x20 = x1.(Succ)._0
    print__T_int32(2)
    goto b19
    b22:
    print__T_int32(6)
    goto b19
    b23:
    print__T_int32(6)
    goto b19
    b24:
    switch x1.(type) {
    case Zero:
        goto b26
    case Succ:
        goto b27
    case Add:
        goto b28
    case Mul:
        goto b29
    default:
        panic("non-exhaustive match")
    }
    b25:
    goto b5
    b26:
    print__T_int32(6)
    goto b25
    b27:
    x25 = x1.(Succ)._0
    print__T_int32(2)
    goto b25
    b28:
    print__T_int32(6)
    goto b25
    b29:
    print__T_int32(6)
    goto b25
    b30:
    x3 = a__0.(Mul)._0
    x4 = a__0.(Mul)._1
    switch x3.(type) {
    case Zero:
        goto b32
    case Succ:
        goto b33
    case Add:
        goto b39
    case Mul:
        goto b45
    default:
        panic("non-exhaustive match")
    }
    b31:
    goto b1
    b32:
    print__T_int32(1)
    goto b31
    b33:
    switch x4.(type) {
    case Zero:
        goto b35
    case Succ:
        goto b36
    case Add:
        goto b37
    case Mul:
        goto b38
    default:
        panic("non-exhaustive match")
    }
    b34:
    goto b31
    b35:
    print__T_int32(3)
    goto b34
    b36:
    print__T_int32(6)
    goto b34
    b37:
    print__T_int32(6)
    goto b34
    b38:
    print__T_int32(6)
    goto b34
    b39:
    x31 = x3.(Add)._0
    x32 = x3.(Add)._1
    switch x4.(type) {
    case Zero:
        goto b41
    case Succ:
        goto b42
    case Add:
        goto b43
    case Mul:
        goto b44
    default:
        panic("non-exhaustive match")
    }
    b40:
    goto b31
    b41:
    print__T_int32(3)
    goto b40
    b42:
    print__T_int32(4)
    goto b40
    b43:
    print__T_int32(4)
    goto b40
    b44:
    print__T_int32(4)
    goto b40
    b45:
    switch x4.(type) {
    case Zero:
        goto b47
    case Succ:
        goto b48
    case Add:
        goto b49
    case Mul:
        goto b50
    default:
        panic("non-exhaustive match")
    }
    b46:
    goto b31
    b47:
    print__T_int32(3)
    goto b46
    b48:
    print__T_int32(6)
    goto b46
    b49:
    print__T_int32(6)
    goto b46
    b50:
    print__T_int32(6)
    goto b46
}

func print__T_int32(value__0 int32) struct{} {
    var t92 string
    var t93 struct{}
    t92 = int32_to_string(value__0)
    t93 = string_print(t92)
    return t93
}

func main() {
    main0()
}
