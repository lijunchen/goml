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
    var t112 Expr = Add{
        _0: Zero{},
        _1: Zero{},
    }
    var a__0 Expr = Mul{
        _0: t112,
        _1: Zero{},
    }
    switch a__0.(type) {
    case Zero:
        print__T_int32(6)
    case Succ:
        print__T_int32(6)
    case Add:
        var x62 Expr = a__0.(Add)._0
        var x63 Expr = a__0.(Add)._1
        switch x63.(type) {
        case Zero:
            switch x62.(type) {
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
            switch x62.(type) {
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
            switch x62.(type) {
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
            switch x62.(type) {
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
        var x64 Expr = a__0.(Mul)._0
        var x65 Expr = a__0.(Mul)._1
        switch x64.(type) {
        case Zero:
            print__T_int32(1)
        case Succ:
            switch x65.(type) {
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
            switch x65.(type) {
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
            switch x65.(type) {
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
    var t155 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t155)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv158 string
    var t159 string = _goml_runtime_core_int32_to_string(self__41)
    retv158 = t159
    return retv158
}

func main() {
    main0()
}
