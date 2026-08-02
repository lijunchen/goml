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
        var inline217 int = 3
        var inline218 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline217)
        _goml_runtime_core_string_print(inline218)
        return struct{}{}
    default:
        var inline221 int = 4
        var inline222 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline221)
        _goml_runtime_core_string_print(inline222)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t215 string = _goml_runtime_core_int_to_string(self__40)
    return t215
}

func main() {
    main0()
}
