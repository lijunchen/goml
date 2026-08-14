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

type Ordering int32

func _goml_m_trait__impl_i_Label_i_string_i_label(self__0 string) string {
    var t411 string = "label:" + self__0
    return t411
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t414 string = self__1.value
    return t414
}

func main0() struct{} {
    var t416 TextSource = TextSource{
        value: "goml",
    }
    var t417 string
    var inline431 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t416)
    var inline432 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline431)
    t417 = inline432
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline428)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
