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
    var retv69 bool
    var t72 bool = x__0 < y__1
    var t73 bool = !t72
    var jp71 bool
    if t73 {
        var t74 bool = y__1 < x__0
        var t75 bool = !t74
        jp71 = t75
    } else {
        jp71 = false
    }
    retv69 = jp71
    return retv69
}

func sum(n__2 int32) int32 {
    var retv77 int32
    var t80 bool = my_int_equal(n__2, 1)
    var jp79 int32
    if t80 {
        jp79 = 1
    } else {
        var t81 int32 = n__2 - 1
        var t82 int32 = sum(t81)
        var t83 int32 = n__2 + t82
        jp79 = t83
    }
    retv77 = jp79
    return retv77
}

func main0() struct{} {
    var t85 int32 = sum(100)
    println__T_int32(t85)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__43)
    retv91 = t92
    return retv91
}

func main() {
    main0()
}
