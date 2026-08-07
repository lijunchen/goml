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
    var mtmp172 bool = x__0 < 2
    switch mtmp172 {
    case true:
        return 1
    case false:
        var t178 int32 = x__0 - 1
        var t179 int32 = fib(t178)
        var t180 int32 = x__0 - 2
        var t181 int32 = fib(t180)
        var t182 int32 = t179 + t181
        return t182
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t184 int32 = fib(10)
    var inline192 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t184)
    _goml_runtime_core_string_print(inline192)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t190 string = _goml_runtime_core_int32_to_string(self__72)
    return t190
}

func main() {
    main0()
}
