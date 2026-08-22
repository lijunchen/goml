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

type Ordering int32

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
    var x415 Expr = Zero{}
    switch x415.(type) {
    case Zero:
        var inline473 int = 3
        var inline474 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline473)
        _goml_runtime_core_string_print(inline474)
        return struct{}{}
    default:
        var inline477 int = 4
        var inline478 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline477)
        _goml_runtime_core_string_print(inline478)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t471 string = _goml_runtime_core_int_to_string(self__151)
    return t471
}

func main() {
    main0()
}
