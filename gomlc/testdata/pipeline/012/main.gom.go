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
    var retv155 int32
    var mtmp152 bool = x__0 < 2
    var jp157 int32
    switch mtmp152 {
    case true:
        jp157 = 1
    case false:
        var t158 int32 = x__0 - 1
        var t159 int32 = fib(t158)
        var t160 int32 = x__0 - 2
        var t161 int32 = fib(t160)
        var t162 int32 = t159 + t161
        jp157 = t162
    default:
        panic("non-exhaustive match")
    }
    retv155 = jp157
    return retv155
}

func main0() struct{} {
    var t164 int32 = fib(10)
    print__T_int32(t164)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int32_to_string(self__43)
    retv169 = t170
    return retv169
}

func main() {
    main0()
}
