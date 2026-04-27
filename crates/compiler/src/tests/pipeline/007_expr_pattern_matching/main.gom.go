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

type GoError = error

func main0() struct{} {
    var t51 Expr = Add{
        _0: Zero{},
        _1: Zero{},
    }
    var a__0 Expr = Mul{
        _0: t51,
        _1: Zero{},
    }
    switch a__0.(type) {
    case Zero:
        print__T_int32(6)
    case Succ:
        print__T_int32(6)
    case Add:
        var x1 Expr = a__0.(Add)._0
        var x2 Expr = a__0.(Add)._1
        switch x2.(type) {
        case Zero:
            switch x1.(type) {
            case Zero:
                print__T_int32(0)
            case Succ:
                print__T_int32(2)
            case Add:
                print__T_int32(5)
            case Mul:
                print__T_int32(5)
            default:
                panic("non-exhaustive match")
            }
        case Succ:
            switch x1.(type) {
            case Zero:
                print__T_int32(6)
            case Succ:
                print__T_int32(2)
            case Add:
                print__T_int32(6)
            case Mul:
                print__T_int32(6)
            default:
                panic("non-exhaustive match")
            }
        case Add:
            switch x1.(type) {
            case Zero:
                print__T_int32(6)
            case Succ:
                print__T_int32(2)
            case Add:
                print__T_int32(6)
            case Mul:
                print__T_int32(6)
            default:
                panic("non-exhaustive match")
            }
        case Mul:
            switch x1.(type) {
            case Zero:
                print__T_int32(6)
            case Succ:
                print__T_int32(2)
            case Add:
                print__T_int32(6)
            case Mul:
                print__T_int32(6)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case Mul:
        var x3 Expr = a__0.(Mul)._0
        var x4 Expr = a__0.(Mul)._1
        switch x3.(type) {
        case Zero:
            print__T_int32(1)
        case Succ:
            switch x4.(type) {
            case Zero:
                print__T_int32(3)
            case Succ:
                print__T_int32(6)
            case Add:
                print__T_int32(6)
            case Mul:
                print__T_int32(6)
            default:
                panic("non-exhaustive match")
            }
        case Add:
            switch x4.(type) {
            case Zero:
                print__T_int32(3)
            case Succ:
                print__T_int32(4)
            case Add:
                print__T_int32(4)
            case Mul:
                print__T_int32(4)
            default:
                panic("non-exhaustive match")
            }
        case Mul:
            switch x4.(type) {
            case Zero:
                print__T_int32(3)
            case Succ:
                print__T_int32(6)
            case Add:
                print__T_int32(6)
            case Mul:
                print__T_int32(6)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t94 string = int32_to_string(value__0)
    string_print(t94)
    return struct{}{}
}

func main() {
    main0()
}
