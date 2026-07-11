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
    var retv10 int32
    var mtmp7 bool = x__0 < 2
    var jp12 int32
    switch mtmp7 {
    case true:
        jp12 = 1
    case false:
        var t13 int32 = x__0 - 1
        var t14 int32 = fib(t13)
        var t15 int32 = x__0 - 2
        var t16 int32 = fib(t15)
        var t17 int32 = t14 + t16
        jp12 = t17
    default:
        panic("non-exhaustive match")
    }
    retv10 = jp12
    return retv10
}

func main0() struct{} {
    var t19 int32 = fib(10)
    print__T_int32(t19)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv24 string
    var t25 string = _goml_runtime_core_int32_to_string(self__13)
    retv24 = t25
    return retv24
}

func main() {
    main0()
}
