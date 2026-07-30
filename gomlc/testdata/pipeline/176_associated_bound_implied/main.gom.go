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
    var retv110 string
    var t111 string = "label:" + self__0
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv113 string
    var t114 string = self__1.value
    retv113 = t114
    return retv113
}

func main0() struct{} {
    var t116 TextSource = TextSource{
        value: "goml",
    }
    var t117 string = render__S_TextSource(t116)
    println__T_string(t117)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv122 string
    var t123 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t124 string = _goml_m_trait__impl_i_Label_i_string_i_label(t123)
    retv122 = t124
    return retv122
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv126 string
    retv126 = self__38
    return retv126
}

func main() {
    main0()
}
