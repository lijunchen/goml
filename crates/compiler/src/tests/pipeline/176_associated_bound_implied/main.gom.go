package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type TextSource struct {
    value string
}

func _goml_m_trait__impl_i_Label_i_string_i_label(self__0 string) string {
    var retv24 string
    var t25 string = "label:" + self__0
    retv24 = t25
    return retv24
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv27 string
    var t28 string = self__1.value
    retv27 = t28
    return retv27
}

func main0() struct{} {
    var t30 TextSource = TextSource{
        value: "goml",
    }
    var t31 string = render__S_TextSource(t30)
    println__T_string(t31)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv36 string
    var t37 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t38 string = _goml_m_trait__impl_i_Label_i_string_i_label(t37)
    retv36 = t38
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv40 string
    retv40 = self__9
    return retv40
}

func main() {
    main0()
}
