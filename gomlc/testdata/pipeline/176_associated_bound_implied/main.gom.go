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
    var t180 string = "label:" + self__0
    return t180
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t183 string = self__1.value
    return t183
}

func main0() struct{} {
    var t185 TextSource = TextSource{
        value: "goml",
    }
    var t186 string
    var inline200 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t185)
    var inline201 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline200)
    t186 = inline201
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
