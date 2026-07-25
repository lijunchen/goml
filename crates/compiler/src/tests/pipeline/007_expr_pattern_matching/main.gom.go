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
    var t100 Expr = Add{
        _0: Zero{},
        _1: Zero{},
    }
    var a__0 Expr = Mul{
        _0: t100,
        _1: Zero{},
    }
    switch a__0.(type) {
    case Add:
        var x65 Expr = a__0.(Add)._0
        var x66 Expr = a__0.(Add)._1
        switch x66.(type) {
        case Zero:
            switch x65.(type) {
            case Zero:
                print__T_int(0)
            case Succ:
                print__T_int(2)
            default:
                print__T_int(5)
            }
        default:
            switch x65.(type) {
            case Succ:
                print__T_int(2)
            default:
                print__T_int(6)
            }
        }
    case Mul:
        var x67 Expr = a__0.(Mul)._0
        var x68 Expr = a__0.(Mul)._1
        switch x67.(type) {
        case Zero:
            print__T_int(1)
        case Add:
            switch x68.(type) {
            case Zero:
                print__T_int(3)
            default:
                print__T_int(4)
            }
        default:
            switch x68.(type) {
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
    var t120 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t120)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int_to_string(self__40)
    retv123 = t124
    return retv123
}

func main() {
    main0()
}
