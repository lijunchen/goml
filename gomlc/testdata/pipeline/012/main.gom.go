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
    var mtmp136 bool = x__0 < 2
    switch mtmp136 {
    case true:
        return 1
    case false:
        var t142 int32 = x__0 - 1
        var t143 int32 = fib(t142)
        var t144 int32 = x__0 - 2
        var t145 int32 = fib(t144)
        var t146 int32 = t143 + t145
        return t146
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t148 int32 = fib(10)
    var inline156 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t148)
    _goml_runtime_core_string_print(inline156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t154 string = _goml_runtime_core_int32_to_string(self__72)
    return t154
}

func main() {
    main0()
}
