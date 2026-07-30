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
    var retv111 int32
    var mtmp108 bool = x__0 < 2
    var jp113 int32
    switch mtmp108 {
    case true:
        jp113 = 1
    case false:
        var t114 int32 = x__0 - 1
        var t115 int32 = fib(t114)
        var t116 int32 = x__0 - 2
        var t117 int32 = fib(t116)
        var t118 int32 = t115 + t117
        jp113 = t118
    default:
        panic("non-exhaustive match")
    }
    retv111 = jp113
    return retv111
}

func main0() struct{} {
    var t120 int32 = fib(10)
    print__T_int32(t120)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int32_to_string(self__43)
    retv125 = t126
    return retv125
}

func main() {
    main0()
}
