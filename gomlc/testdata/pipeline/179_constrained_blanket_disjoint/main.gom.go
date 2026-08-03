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
    var inline204 string = _goml_runtime_core_int32_to_string(self__0)
    return inline204
}

func main0() struct{} {
    var t188 string
    var inline216 string = "text"
    var inline217 string = "string:" + inline216
    t188 = inline217
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline213)
    var t189 string
    var inline209 int32 = 7
    var inline210 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline209)
    var inline211 string = "blanket:" + inline210
    t189 = inline211
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
