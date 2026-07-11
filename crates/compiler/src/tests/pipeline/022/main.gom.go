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
    var retv13 int32
    var jp15 int32
    switch s__0 {
    case "hello":
        jp15 = 1
    case "world":
        jp15 = 2
    default:
        jp15 = 3
    }
    retv13 = jp15
    return retv13
}

func wildcard_position(s__1 string) int32 {
    var retv17 int32
    retv17 = 4
    return retv17
}

func repeated_string(s__2 string) int32 {
    var retv19 int32
    var jp21 int32
    switch s__2 {
    case "hello":
        jp21 = 6
    default:
        jp21 = 8
    }
    retv19 = jp21
    return retv19
}

func main0() struct{} {
    var t23 int32 = match_string("hello")
    var t24 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t23)
    println__T_string(t24)
    var t25 int32 = match_string("planet")
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t25)
    println__T_string(t26)
    var t27 int32 = wildcard_position("world")
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t27)
    println__T_string(t28)
    var t29 int32 = wildcard_position("sun")
    var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t29)
    println__T_string(t30)
    var t31 int32 = repeated_string("hello")
    var t32 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t31)
    println__T_string(t32)
    var t33 int32 = repeated_string("mars")
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t33)
    println__T_string(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t37 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t37)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv43 string
    retv43 = self__9
    return retv43
}

func main() {
    main0()
}
