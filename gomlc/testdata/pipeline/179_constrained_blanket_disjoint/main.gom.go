package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Box__string struct {
    value string
}

type Box__int32 struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var inline182 string = _goml_runtime_core_int32_to_string(self__0)
    return inline182
}

func main0() struct{} {
    var t166 string
    var inline194 string = "text"
    var inline195 string = "string:" + inline194
    t166 = inline195
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline191)
    var t167 string
    var inline187 int32 = 7
    var inline188 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline187)
    var inline189 string = "blanket:" + inline188
    t167 = inline189
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
