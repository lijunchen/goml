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
    var t139 string = "label:" + self__0
    return t139
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t142 string = self__1.value
    return t142
}

func main0() struct{} {
    var t144 TextSource = TextSource{
        value: "goml",
    }
    var t145 string
    var inline159 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t144)
    var inline160 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline159)
    t145 = inline160
    var inline156 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
