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

func match_string(s__0 string) int32 {
    var retv10 int32
    var jp12 int32
    switch s__0 {
    case "hello":
        jp12 = 1
    case "world":
        jp12 = 2
    default:
        jp12 = 3
    }
    retv10 = jp12
    return retv10
}

func wildcard_position(s__1 string) int32 {
    var retv14 int32
    retv14 = 4
    return retv14
}

func repeated_string(s__2 string) int32 {
    var retv16 int32
    var jp18 int32
    switch s__2 {
    case "hello":
        jp18 = 6
    default:
        jp18 = 8
    }
    retv16 = jp18
    return retv16
}

func main0() struct{} {
    var t20 int32 = match_string("hello")
    var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t20)
    println__T_string(t21)
    var t22 int32 = match_string("planet")
    var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t22)
    println__T_string(t23)
    var t24 int32 = wildcard_position("world")
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t24)
    println__T_string(t25)
    var t26 int32 = wildcard_position("sun")
    var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t26)
    println__T_string(t27)
    var t28 int32 = repeated_string("hello")
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t28)
    println__T_string(t29)
    var t30 int32 = repeated_string("mars")
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t30)
    println__T_string(t31)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int32_to_string(self__2)
    retv37 = t38
    return retv37
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv40 string
    retv40 = self__9
    return retv40
}

func main() {
    main0()
}
