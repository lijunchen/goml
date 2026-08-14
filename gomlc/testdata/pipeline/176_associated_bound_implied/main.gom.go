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
    var t190 string = "label:" + self__0
    return t190
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t193 string = self__1.value
    return t193
}

func main0() struct{} {
    var t195 TextSource = TextSource{
        value: "goml",
    }
    var t196 string
    var inline210 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t195)
    var inline211 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline210)
    t196 = inline211
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
