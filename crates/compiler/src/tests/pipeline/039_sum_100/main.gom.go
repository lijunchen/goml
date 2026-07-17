package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func my_int_equal(x__0 int32, y__1 int32) bool {
    var retv59 bool
    var t62 bool = x__0 < y__1
    var t63 bool = !t62
    var jp61 bool
    if t63 {
        var t64 bool = y__1 < x__0
        var t65 bool = !t64
        jp61 = t65
    } else {
        jp61 = false
    }
    retv59 = jp61
    return retv59
}

func sum(n__2 int32) int32 {
    var retv67 int32
    var t70 bool = my_int_equal(n__2, 1)
    var jp69 int32
    if t70 {
        jp69 = 1
    } else {
        var t71 int32 = n__2 - 1
        var t72 int32 = sum(t71)
        var t73 int32 = n__2 + t72
        jp69 = t73
    }
    retv67 = jp69
    return retv67
}

func main0() struct{} {
    var t75 int32 = sum(100)
    println__T_int32(t75)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__38)
    retv81 = t82
    return retv81
}

func main() {
    main0()
}
