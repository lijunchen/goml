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
    var inline199 string = _goml_runtime_core_int32_to_string(self__0)
    return inline199
}

func main0() struct{} {
    var t183 string
    var inline211 string = "text"
    var inline212 string = "string:" + inline211
    t183 = inline212
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline208)
    var t184 string
    var inline204 int32 = 7
    var inline205 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline204)
    var inline206 string = "blanket:" + inline205
    t184 = inline206
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
