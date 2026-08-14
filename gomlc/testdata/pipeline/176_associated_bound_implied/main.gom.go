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
    var t185 string = "label:" + self__0
    return t185
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t188 string = self__1.value
    return t188
}

func main0() struct{} {
    var t190 TextSource = TextSource{
        value: "goml",
    }
    var t191 string
    var inline205 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t190)
    var inline206 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline205)
    t191 = inline206
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
