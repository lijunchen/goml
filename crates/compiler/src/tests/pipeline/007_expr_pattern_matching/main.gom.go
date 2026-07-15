package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
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
    var t73 Expr = Add{
        _0: Zero{},
        _1: Zero{},
    }
    var a__0 Expr = Mul{
        _0: t73,
        _1: Zero{},
    }
    switch a__0.(type) {
    case Zero:
        print__T_int32(6)
    case Succ:
        print__T_int32(6)
    case Add:
        var x23 Expr = a__0.(Add)._0
        var x24 Expr = a__0.(Add)._1
        switch x24.(type) {
        case Zero:
            switch x23.(type) {
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
            switch x23.(type) {
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
            switch x23.(type) {
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
            switch x23.(type) {
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
        var x25 Expr = a__0.(Mul)._0
        var x26 Expr = a__0.(Mul)._1
        switch x25.(type) {
        case Zero:
            print__T_int32(1)
        case Succ:
            switch x26.(type) {
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
            switch x26.(type) {
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
            switch x26.(type) {
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
    var t116 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t116)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv119 string
    var t120 string = _goml_runtime_core_int32_to_string(self__13)
    retv119 = t120
    return retv119
}

func main() {
    main0()
}
