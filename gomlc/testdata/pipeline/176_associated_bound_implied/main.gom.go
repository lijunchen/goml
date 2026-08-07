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
    var t175 string = "label:" + self__0
    return t175
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t178 string = self__1.value
    return t178
}

func main0() struct{} {
    var t180 TextSource = TextSource{
        value: "goml",
    }
    var t181 string
    var inline195 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t180)
    var inline196 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline195)
    t181 = inline196
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline192)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
