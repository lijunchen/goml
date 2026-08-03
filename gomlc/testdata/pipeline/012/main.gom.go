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
    var mtmp177 bool = x__0 < 2
    switch mtmp177 {
    case true:
        return 1
    case false:
        var t183 int32 = x__0 - 1
        var t184 int32 = fib(t183)
        var t185 int32 = x__0 - 2
        var t186 int32 = fib(t185)
        var t187 int32 = t184 + t186
        return t187
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t189 int32 = fib(10)
    var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t189)
    _goml_runtime_core_string_print(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t195 string = _goml_runtime_core_int32_to_string(self__72)
    return t195
}

func main() {
    main0()
}
