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
    var x140 Expr = Zero{}
    switch x140.(type) {
    case Zero:
        var inline198 int = 3
        var inline199 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline198)
        _goml_runtime_core_string_print(inline199)
        return struct{}{}
    default:
        var inline202 int = 4
        var inline203 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline202)
        _goml_runtime_core_string_print(inline203)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t196 string = _goml_runtime_core_int_to_string(self__69)
    return t196
}

func main() {
    main0()
}
