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
    var t414 string = "label:" + self__0
    return t414
}

func _goml_m_trait__impl_i_Source_i_TextSource_i_get(self__1 TextSource) string {
    var t417 string = self__1.value
    return t417
}

func main0() struct{} {
    var t419 TextSource = TextSource{
        value: "goml",
    }
    var t420 string
    var inline434 string = _goml_m_trait__impl_i_Source_i_TextSource_i_get(t419)
    var inline435 string = _goml_m_trait__impl_i_Label_i_string_i_label(inline434)
    t420 = inline435
    var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline431)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
