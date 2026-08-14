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
    var x186 Expr = Zero{}
    switch x186.(type) {
    case Zero:
        var inline244 int = 3
        var inline245 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline244)
        _goml_runtime_core_string_print(inline245)
        return struct{}{}
    default:
        var inline248 int = 4
        var inline249 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline248)
        _goml_runtime_core_string_print(inline249)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t242 string = _goml_runtime_core_int_to_string(self__67)
    return t242
}

func main() {
    main0()
}
