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
    var t182 int32 = self__0.value
    return t182
}

func main0() struct{} {
    var t184 Number = Number{
        value: 42,
    }
    var t185 int32
    var inline240 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t184)
    t185 = inline240
    var t186 string
    var inline238 string = _goml_runtime_core_int32_to_string(t185)
    t186 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline235)
    var t187 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline233 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t187)
    value__4 = inline233
    var t188 string
    var inline231 string = _goml_runtime_core_int32_to_string(value__4)
    t188 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline228)
    var t189 Box__string = Box__string{
        value: "generic",
    }
    var t190 string
    var inline226 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t189)
    t190 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline223)
    var t192 int32
    var inline221 int32 = 11
    t192 = inline221
    var t193 string
    var inline219 string = _goml_runtime_core_int32_to_string(t192)
    t193 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t214 string = self__1.value
    return t214
}

func main() {
    main0()
}
