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
    var retv5 bool
    var t8 bool = x__0 < y__1
    var t9 bool = !t8
    var jp7 bool
    if t9 {
        var t10 bool = y__1 < x__0
        var t11 bool = !t10
        jp7 = t11
    } else {
        jp7 = false
    }
    retv5 = jp7
    return retv5
}

func sum(n__2 int32) int32 {
    var retv13 int32
    var t16 bool = my_int_equal(n__2, 1)
    var jp15 int32
    if t16 {
        jp15 = 1
    } else {
        var t17 int32 = n__2 - 1
        var t18 int32 = sum(t17)
        var t19 int32 = n__2 + t18
        jp15 = t19
    }
    retv13 = jp15
    return retv13
}

func main0() struct{} {
    var t21 int32 = sum(100)
    println__T_int32(t21)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t24 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t24)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv27 string
    var t28 string = _goml_runtime_core_int32_to_string(self__13)
    retv27 = t28
    return retv27
}

func main() {
    main0()
}
