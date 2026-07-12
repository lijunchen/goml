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

type NumberBox struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var retv24 string
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t26 string = "marked:" + t25
    retv24 = t26
    return retv24
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv28 int32
    var t29 int32 = self__1.value
    retv28 = t29
    return retv28
}

func main0() struct{} {
    var t31 NumberBox = NumberBox{
        value: 7,
    }
    var t32 string = describe__C_NumberBox__T_int32(t31)
    println__T_string(t32)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv34 string
    var t35 string = _goml_runtime_core_int32_to_string(self__2)
    retv34 = t35
    return retv34
}

func println__T_string(value__1 string) struct{} {
    var t37 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t37)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv40 string
    var t41 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t42 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t41)
    retv40 = t42
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv44 string
    retv44 = self__9
    return retv44
}

func main() {
    main0()
}
