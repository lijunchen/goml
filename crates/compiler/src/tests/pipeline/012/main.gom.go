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
    var retv25 int32
    var mtmp22 bool = x__0 < 2
    var jp27 int32
    switch mtmp22 {
    case true:
        jp27 = 1
    case false:
        var t28 int32 = x__0 - 1
        var t29 int32 = fib(t28)
        var t30 int32 = x__0 - 2
        var t31 int32 = fib(t30)
        var t32 int32 = t29 + t31
        jp27 = t32
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t34 int32 = fib(10)
    print__T_int32(t34)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv39 string
    var t40 string = _goml_runtime_core_int32_to_string(self__13)
    retv39 = t40
    return retv39
}

func main() {
    main0()
}
