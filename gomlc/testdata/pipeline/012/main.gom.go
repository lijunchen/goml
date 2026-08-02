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
    var mtmp155 bool = x__0 < 2
    switch mtmp155 {
    case true:
        return 1
    case false:
        var t161 int32 = x__0 - 1
        var t162 int32 = fib(t161)
        var t163 int32 = x__0 - 2
        var t164 int32 = fib(t163)
        var t165 int32 = t162 + t164
        return t165
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t167 int32 = fib(10)
    print__T_int32(t167)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t173 string = _goml_runtime_core_int32_to_string(self__43)
    return t173
}

func main() {
    main0()
}
