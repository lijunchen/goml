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
    var mtmp182 bool = x__0 < 2
    switch mtmp182 {
    case true:
        return 1
    case false:
        var t188 int32 = x__0 - 1
        var t189 int32 = fib(t188)
        var t190 int32 = x__0 - 2
        var t191 int32 = fib(t190)
        var t192 int32 = t189 + t191
        return t192
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t194 int32 = fib(10)
    var inline202 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
    _goml_runtime_core_string_print(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t200 string = _goml_runtime_core_int32_to_string(self__70)
    return t200
}

func main() {
    main0()
}
