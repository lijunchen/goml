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

type Number struct {
    value int32
}

type Box__string struct {
    value string
}

func _goml_m_trait__impl_i_Provider_i_Number_i_get(self__0 Number) int32 {
    var t187 int32 = self__0.value
    return t187
}

func main0() struct{} {
    var t189 Number = Number{
        value: 42,
    }
    var t190 int32
    var inline245 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t189)
    t190 = inline245
    var t191 string
    var inline243 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline240)
    var t192 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline238 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t192)
    value__4 = inline238
    var t193 string
    var inline236 string = _goml_runtime_core_int32_to_string(value__4)
    t193 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline233)
    var t194 Box__string = Box__string{
        value: "generic",
    }
    var t195 string
    var inline231 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t194)
    t195 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline228)
    var t197 int32
    var inline226 int32 = 11
    t197 = inline226
    var t198 string
    var inline224 string = _goml_runtime_core_int32_to_string(t197)
    t198 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t219 string = self__1.value
    return t219
}

func main() {
    main0()
}
