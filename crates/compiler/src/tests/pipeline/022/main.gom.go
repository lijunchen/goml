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
    var retv28 int32
    var jp30 int32
    switch s__0 {
    case "hello":
        jp30 = 1
    case "world":
        jp30 = 2
    default:
        jp30 = 3
    }
    retv28 = jp30
    return retv28
}

func wildcard_position(s__1 string) int32 {
    var retv32 int32
    retv32 = 4
    return retv32
}

func repeated_string(s__2 string) int32 {
    var retv34 int32
    var jp36 int32
    switch s__2 {
    case "hello":
        jp36 = 6
    default:
        jp36 = 8
    }
    retv34 = jp36
    return retv34
}

func main0() struct{} {
    var t38 int32 = match_string("hello")
    var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t38)
    println__T_string(t39)
    var t40 int32 = match_string("planet")
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t40)
    println__T_string(t41)
    var t42 int32 = wildcard_position("world")
    var t43 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t42)
    println__T_string(t43)
    var t44 int32 = wildcard_position("sun")
    var t45 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t44)
    println__T_string(t45)
    var t46 int32 = repeated_string("hello")
    var t47 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t46)
    println__T_string(t47)
    var t48 int32 = repeated_string("mars")
    var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t48)
    println__T_string(t49)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t52 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t52)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv55 string
    var t56 string = _goml_runtime_core_int32_to_string(self__2)
    retv55 = t56
    return retv55
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv58 string
    retv58 = self__9
    return retv58
}

func main() {
    main0()
}
