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
    var t158 string = "label:" + self__0
    return t158
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t161 string = self__1.value
    return t161
}

func main0() struct{} {
    var t163 TextSource = TextSource{
        value: "goml",
    }
    var t164 string = render__S_TextSource(t163)
    println__T_string(t164)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var t170 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t171 string = _goml_m_trait__impl_i_Label_i_string_i_label(t170)
    return t171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
