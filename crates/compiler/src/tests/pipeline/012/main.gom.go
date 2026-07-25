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
    var retv67 int32
    var mtmp64 bool = x__0 < 2
    var jp69 int32
    switch mtmp64 {
    case true:
        jp69 = 1
    case false:
        var t70 int32 = x__0 - 1
        var t71 int32 = fib(t70)
        var t72 int32 = x__0 - 2
        var t73 int32 = fib(t72)
        var t74 int32 = t71 + t73
        jp69 = t74
    default:
        panic("non-exhaustive match")
    }
    retv67 = jp69
    return retv67
}

func main0() struct{} {
    var t76 int32 = fib(10)
    print__T_int32(t76)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t78)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__43)
    retv81 = t82
    return retv81
}

func main() {
    main0()
}
