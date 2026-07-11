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
    var retv23 bool
    var t26 bool = x__0 < y__1
    var t27 bool = !t26
    var jp25 bool
    if t27 {
        var t28 bool = y__1 < x__0
        var t29 bool = !t28
        jp25 = t29
    } else {
        jp25 = false
    }
    retv23 = jp25
    return retv23
}

func sum(n__2 int32) int32 {
    var retv31 int32
    var t34 bool = my_int_equal(n__2, 1)
    var jp33 int32
    if t34 {
        jp33 = 1
    } else {
        var t35 int32 = n__2 - 1
        var t36 int32 = sum(t35)
        var t37 int32 = n__2 + t36
        jp33 = t37
    }
    retv31 = jp33
    return retv31
}

func main0() struct{} {
    var t39 int32 = sum(100)
    println__T_int32(t39)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t42 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t42)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv45 string
    var t46 string = _goml_runtime_core_int32_to_string(self__13)
    retv45 = t46
    return retv45
}

func main() {
    main0()
}
