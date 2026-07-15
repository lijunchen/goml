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

type Box__string struct {
    value string
}

type Box__int32 struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var retv25 string
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv25 = t26
    return retv25
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv28 string
    var t29 string = self__2.value
    var t30 string = "string:" + t29
    retv28 = t30
    return retv28
}

func main0() struct{} {
    var t32 Box__string = Box__string{
        value: "text",
    }
    var t33 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t32)
    println__T_string(t33)
    var t34 Box__int32 = Box__int32{
        value: 7,
    }
    var t35 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(t34)
    println__T_string(t35)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int32_to_string(self__2)
    retv37 = t38
    return retv37
}

func println__T_string(value__1 string) struct{} {
    var t40 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t40)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv43 string
    var t44 int32 = self__1.value
    var t45 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t44)
    var t46 string = "blanket:" + t45
    retv43 = t46
    return retv43
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv48 string
    retv48 = self__9
    return retv48
}

func main() {
    main0()
}
