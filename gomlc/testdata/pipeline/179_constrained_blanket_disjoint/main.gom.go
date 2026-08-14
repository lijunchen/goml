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
    var inline214 string = _goml_runtime_core_int32_to_string(self__0)
    return inline214
}

func main0() struct{} {
    var t198 string
    var inline226 string = "text"
    var inline227 string = "string:" + inline226
    t198 = inline227
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline223)
    var t199 string
    var inline219 int32 = 7
    var inline220 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline219)
    var inline221 string = "blanket:" + inline220
    t199 = inline221
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
