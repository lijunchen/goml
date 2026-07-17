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
    var retv64 int32
    var mtmp61 bool = x__0 < 2
    var jp66 int32
    switch mtmp61 {
    case true:
        jp66 = 1
    case false:
        var t67 int32 = x__0 - 1
        var t68 int32 = fib(t67)
        var t69 int32 = x__0 - 2
        var t70 int32 = fib(t69)
        var t71 int32 = t68 + t70
        jp66 = t71
    default:
        panic("non-exhaustive match")
    }
    retv64 = jp66
    return retv64
}

func main0() struct{} {
    var t73 int32 = fib(10)
    print__T_int32(t73)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__41)
    retv78 = t79
    return retv78
}

func main() {
    main0()
}
