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

type _goml_m_packages__basic_p_Lib_p_Color int32

const (
    Red _goml_m_packages__basic_p_Lib_p_Color = 0
    Green _goml_m_packages__basic_p_Lib_p_Color = 1
)

func _goml_m_packages__basic_p_main() struct{} {
    var t5 int32 = _goml_m_packages__basic_p_Lib_p_color__to__int(Red)
    var t6 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t5)
    println__T_string(t6)
    return struct{}{}
}

func _goml_m_packages__basic_p_Lib_p_color__to__int(c__0 _goml_m_packages__basic_p_Lib_p_Color) int32 {
    var retv9 int32
    var jp11 int32
    switch c__0 {
    case Red:
        jp11 = 1
    case Green:
        jp11 = 2
    default:
        panic("non-exhaustive match")
    }
    retv9 = jp11
    return retv9
}

func main0() struct{} {
    _goml_m_packages__basic_p_main()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t15 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t15)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv18 string
    var t19 string = _goml_runtime_core_int32_to_string(self__2)
    retv18 = t19
    return retv18
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv21 string
    retv21 = self__9
    return retv21
}

func main() {
    main0()
}
