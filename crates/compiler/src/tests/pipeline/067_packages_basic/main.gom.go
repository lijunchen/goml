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
    var t62 int32 = _goml_m_packages__basic_p_Lib_p_color__to__int(Red)
    var t63 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t62)
    println__T_string(t63)
    return struct{}{}
}

func _goml_m_packages__basic_p_Lib_p_color__to__int(c__0 _goml_m_packages__basic_p_Lib_p_Color) int32 {
    var retv66 int32
    var jp68 int32
    switch c__0 {
    case Red:
        jp68 = 1
    case Green:
        jp68 = 2
    default:
        panic("non-exhaustive match")
    }
    retv66 = jp68
    return retv66
}

func main0() struct{} {
    _goml_m_packages__basic_p_main()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__5)
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv78 string
    retv78 = self__37
    return retv78
}

func main() {
    main0()
}
