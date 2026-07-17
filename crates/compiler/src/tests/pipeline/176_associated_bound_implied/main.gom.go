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
    var retv60 string
    var t61 string = "label:" + self__0
    retv60 = t61
    return retv60
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv63 string
    var t64 string = self__1.value
    retv63 = t64
    return retv63
}

func main0() struct{} {
    var t66 TextSource = TextSource{
        value: "goml",
    }
    var t67 string = render__S_TextSource(t66)
    println__T_string(t67)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv72 string
    var t73 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t74 string = _goml_m_trait__impl_i_Label_i_string_i_label(t73)
    retv72 = t74
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv76 string
    retv76 = self__34
    return retv76
}

func main() {
    main0()
}
