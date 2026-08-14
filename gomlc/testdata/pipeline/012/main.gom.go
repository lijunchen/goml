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
    var mtmp187 bool = x__0 < 2
    switch mtmp187 {
    case true:
        return 1
    case false:
        var t193 int32 = x__0 - 1
        var t194 int32 = fib(t193)
        var t195 int32 = x__0 - 2
        var t196 int32 = fib(t195)
        var t197 int32 = t194 + t196
        return t197
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t199 int32 = fib(10)
    var inline207 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
    _goml_runtime_core_string_print(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t205 string = _goml_runtime_core_int32_to_string(self__70)
    return t205
}

func main() {
    main0()
}
