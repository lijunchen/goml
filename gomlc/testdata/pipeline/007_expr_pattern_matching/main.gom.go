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
    var x191 Expr = Zero{}
    switch x191.(type) {
    case Zero:
        var inline249 int = 3
        var inline250 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline249)
        _goml_runtime_core_string_print(inline250)
        return struct{}{}
    default:
        var inline253 int = 4
        var inline254 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline253)
        _goml_runtime_core_string_print(inline254)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t247 string = _goml_runtime_core_int_to_string(self__67)
    return t247
}

func main() {
    main0()
}
