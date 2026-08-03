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
    var inline163 string = _goml_runtime_core_int32_to_string(self__0)
    return inline163
}

func main0() struct{} {
    var t147 string
    var inline175 string = "text"
    var inline176 string = "string:" + inline175
    t147 = inline176
    var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline172)
    var t148 string
    var inline168 int32 = 7
    var inline169 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline168)
    var inline170 string = "blanket:" + inline169
    t148 = inline170
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
