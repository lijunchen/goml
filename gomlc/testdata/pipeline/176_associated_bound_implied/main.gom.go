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
    var retv157 string
    var t158 string = "label:" + self__0
    retv157 = t158
    return retv157
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv160 string
    var t161 string = self__1.value
    retv160 = t161
    return retv160
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
    var retv169 string
    var t170 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t171 string = _goml_m_trait__impl_i_Label_i_string_i_label(t170)
    retv169 = t171
    return retv169
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv173 string
    retv173 = self__38
    return retv173
}

func main() {
    main0()
}
