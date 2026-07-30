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
    var retv70 string
    var t71 string = "label:" + self__0
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var retv73 string
    var t74 string = self__1.value
    retv73 = t74
    return retv73
}

func main0() struct{} {
    var t76 TextSource = TextSource{
        value: "goml",
    }
    var t77 string = render__S_TextSource(t76)
    println__T_string(t77)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t79 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func render__S_TextSource(source__2 TextSource) string {
    var retv82 string
    var t83 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(source__2)
    var t84 string = _goml_m_trait__impl_i_Label_i_string_i_label(t83)
    retv82 = t84
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv86 string
    retv86 = self__38
    return retv86
}

func main() {
    main0()
}
