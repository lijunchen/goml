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

func fib(x__0 int32) int32 {
    var retv61 int32
    var mtmp58 bool = x__0 < 2
    var jp63 int32
    switch mtmp58 {
    case true:
        jp63 = 1
    case false:
        var t64 int32 = x__0 - 1
        var t65 int32 = fib(t64)
        var t66 int32 = x__0 - 2
        var t67 int32 = fib(t66)
        var t68 int32 = t65 + t67
        jp63 = t68
    default:
        panic("non-exhaustive match")
    }
    retv61 = jp63
    return retv61
}

func main0() struct{} {
    var t70 int32 = fib(10)
    print__T_int32(t70)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__38)
    retv75 = t76
    return retv75
}

func main() {
    main0()
}
