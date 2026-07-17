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
    var retv62 bool
    var t65 bool = x__0 < y__1
    var t66 bool = !t65
    var jp64 bool
    if t66 {
        var t67 bool = y__1 < x__0
        var t68 bool = !t67
        jp64 = t68
    } else {
        jp64 = false
    }
    retv62 = jp64
    return retv62
}

func sum(n__2 int32) int32 {
    var retv70 int32
    var t73 bool = my_int_equal(n__2, 1)
    var jp72 int32
    if t73 {
        jp72 = 1
    } else {
        var t74 int32 = n__2 - 1
        var t75 int32 = sum(t74)
        var t76 int32 = n__2 + t75
        jp72 = t76
    }
    retv70 = jp72
    return retv70
}

func main0() struct{} {
    var t78 int32 = sum(100)
    println__T_int32(t78)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__41)
    retv84 = t85
    return retv84
}

func main() {
    main0()
}
