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
    var retv65 bool
    var t68 bool = x__0 < y__1
    var t69 bool = !t68
    var jp67 bool
    if t69 {
        var t70 bool = y__1 < x__0
        var t71 bool = !t70
        jp67 = t71
    } else {
        jp67 = false
    }
    retv65 = jp67
    return retv65
}

func sum(n__2 int32) int32 {
    var retv73 int32
    var t76 bool = my_int_equal(n__2, 1)
    var jp75 int32
    if t76 {
        jp75 = 1
    } else {
        var t77 int32 = n__2 - 1
        var t78 int32 = sum(t77)
        var t79 int32 = n__2 + t78
        jp75 = t79
    }
    retv73 = jp75
    return retv73
}

func main0() struct{} {
    var t81 int32 = sum(100)
    println__T_int32(t81)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__43)
    retv87 = t88
    return retv87
}

func main() {
    main0()
}
