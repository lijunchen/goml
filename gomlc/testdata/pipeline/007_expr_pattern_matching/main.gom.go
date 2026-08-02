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
    var x159 Expr = Zero{}
    switch x159.(type) {
    case Zero:
        print__T_int(3)
        return struct{}{}
    default:
        print__T_int(4)
        return struct{}{}
    }
}

func print__T_int(value__0 int) struct{} {
    var t211 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t215 string = _goml_runtime_core_int_to_string(self__40)
    return t215
}

func main() {
    main0()
}
