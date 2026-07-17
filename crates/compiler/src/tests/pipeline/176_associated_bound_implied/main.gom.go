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
    var retv63 string
    var t64 string = "label:" + self__0
    retv63 = t64
    return retv63
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv66 string
    var t67 string = self__1.value
    retv66 = t67
    return retv66
}

func main0() struct{} {
    var t69 TextSource = TextSource{
        value: "goml",
    }
    var t70 string = render__S_TextSource(t69)
    println__T_string(t70)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv75 string
    var t76 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t77 string = _goml_m_trait__impl_i_Label_i_string_i_label(t76)
    retv75 = t77
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv79 string
    retv79 = self__37
    return retv79
}

func main() {
    main0()
}
