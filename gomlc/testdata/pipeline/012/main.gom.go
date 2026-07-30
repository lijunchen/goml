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
    var retv71 int32
    var mtmp68 bool = x__0 < 2
    var jp73 int32
    switch mtmp68 {
    case true:
        jp73 = 1
    case false:
        var t74 int32 = x__0 - 1
        var t75 int32 = fib(t74)
        var t76 int32 = x__0 - 2
        var t77 int32 = fib(t76)
        var t78 int32 = t75 + t77
        jp73 = t78
    default:
        panic("non-exhaustive match")
    }
    retv71 = jp73
    return retv71
}

func main0() struct{} {
    var t80 int32 = fib(10)
    print__T_int32(t80)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv85 string
    var t86 string = _goml_runtime_core_int32_to_string(self__43)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
