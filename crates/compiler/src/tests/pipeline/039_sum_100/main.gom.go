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
    var retv8 bool
    var t11 bool = x__0 < y__1
    var t12 bool = !t11
    var jp10 bool
    if t12 {
        var t13 bool = y__1 < x__0
        var t14 bool = !t13
        jp10 = t14
    } else {
        jp10 = false
    }
    retv8 = jp10
    return retv8
}

func sum(n__2 int32) int32 {
    var retv16 int32
    var t19 bool = my_int_equal(n__2, 1)
    var jp18 int32
    if t19 {
        jp18 = 1
    } else {
        var t20 int32 = n__2 - 1
        var t21 int32 = sum(t20)
        var t22 int32 = n__2 + t21
        jp18 = t22
    }
    retv16 = jp18
    return retv16
}

func main0() struct{} {
    var t24 int32 = sum(100)
    println__T_int32(t24)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t27 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t27)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int32_to_string(self__13)
    retv30 = t31
    return retv30
}

func main() {
    main0()
}
