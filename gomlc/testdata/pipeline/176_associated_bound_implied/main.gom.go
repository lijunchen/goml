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
    var retv154 string
    var t155 string = "label:" + self__0
    retv154 = t155
    return retv154
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv157 string
    var t158 string = self__1.value
    retv157 = t158
    return retv157
}

func main0() struct{} {
    var t160 TextSource = TextSource{
        value: "goml",
    }
    var t161 string = render__S_TextSource(t160)
    println__T_string(t161)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv166 string
    var t167 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t168 string = _goml_m_trait__impl_i_Label_i_string_i_label(t167)
    retv166 = t168
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv170 string
    retv170 = self__38
    return retv170
}

func main() {
    main0()
}
