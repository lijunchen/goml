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
    var inline209 string = _goml_runtime_core_int32_to_string(self__0)
    return inline209
}

func main0() struct{} {
    var t193 string
    var inline221 string = "text"
    var inline222 string = "string:" + inline221
    t193 = inline222
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline218)
    var t194 string
    var inline214 int32 = 7
    var inline215 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline214)
    var inline216 string = "blanket:" + inline215
    t194 = inline216
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
