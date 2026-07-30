package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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
    var t144 Expr = Add{
        _0: Zero{},
        _1: Zero{},
    }
    var a__0 Expr = Mul{
        _0: t144,
        _1: Zero{},
    }
    switch a__0.(type) {
    case Add:
        var x109 Expr = a__0.(Add)._0
        var x110 Expr = a__0.(Add)._1
        switch x110.(type) {
        case Zero:
            switch x109.(type) {
            case Zero:
                print__T_int(0)
            case Succ:
                print__T_int(2)
            default:
                print__T_int(5)
            }
        default:
            switch x109.(type) {
            case Succ:
                print__T_int(2)
            default:
                print__T_int(6)
            }
        }
    case Mul:
        var x111 Expr = a__0.(Mul)._0
        var x112 Expr = a__0.(Mul)._1
        switch x111.(type) {
        case Zero:
            print__T_int(1)
        case Add:
            switch x112.(type) {
            case Zero:
                print__T_int(3)
            default:
                print__T_int(4)
            }
        default:
            switch x112.(type) {
            case Zero:
                print__T_int(3)
            default:
                print__T_int(6)
            }
        }
    default:
        print__T_int(6)
    }
    return struct{}{}
}

func print__T_int(value__0 int) struct{} {
    var t164 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t164)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv167 string
    var t168 string = _goml_runtime_core_int_to_string(self__40)
    retv167 = t168
    return retv167
}

func main() {
    main0()
}
