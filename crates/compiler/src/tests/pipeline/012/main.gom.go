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
    var retv7 int32
    var mtmp4 bool = x__0 < 2
    var jp9 int32
    switch mtmp4 {
    case true:
        jp9 = 1
    case false:
        var t10 int32 = x__0 - 1
        var t11 int32 = fib(t10)
        var t12 int32 = x__0 - 2
        var t13 int32 = fib(t12)
        var t14 int32 = t11 + t13
        jp9 = t14
    default:
        panic("non-exhaustive match")
    }
    retv7 = jp9
    return retv7
}

func main0() struct{} {
    var t16 int32 = fib(10)
    print__T_int32(t16)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t18 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t18)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv21 string
    var t22 string = _goml_runtime_core_int32_to_string(self__13)
    retv21 = t22
    return retv21
}

func main() {
    main0()
}
