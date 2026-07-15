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
    var t23 int32 = _goml_m_packages__basic_p_Lib_p_color__to__int(Red)
    var t24 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t23)
    println__T_string(t24)
    return struct{}{}
}

func _goml_m_packages__basic_p_Lib_p_color__to__int(c__0 _goml_m_packages__basic_p_Lib_p_Color) int32 {
    var retv27 int32
    var jp29 int32
    switch c__0 {
    case Red:
        jp29 = 1
    case Green:
        jp29 = 2
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func main0() struct{} {
    _goml_m_packages__basic_p_main()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_int32_to_string(self__2)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
