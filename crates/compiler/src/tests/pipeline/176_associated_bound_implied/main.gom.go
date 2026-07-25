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
    var retv66 string
    var t67 string = "label:" + self__0
    retv66 = t67
    return retv66
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv69 string
    var t70 string = self__1.value
    retv69 = t70
    return retv69
}

func main0() struct{} {
    var t72 TextSource = TextSource{
        value: "goml",
    }
    var t73 string = render__S_TextSource(t72)
    println__T_string(t73)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv78 string
    var t79 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t80 string = _goml_m_trait__impl_i_Label_i_string_i_label(t79)
    retv78 = t80
    return retv78
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv82 string
    retv82 = self__38
    return retv82
}

func main() {
    main0()
}
