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
    var t177 int32 = self__0.value
    return t177
}

func main0() struct{} {
    var t179 Number = Number{
        value: 42,
    }
    var t180 int32
    var inline235 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t179)
    t180 = inline235
    var t181 string
    var inline233 string = _goml_runtime_core_int32_to_string(t180)
    t181 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline230)
    var t182 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline228 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t182)
    value__4 = inline228
    var t183 string
    var inline226 string = _goml_runtime_core_int32_to_string(value__4)
    t183 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline223)
    var t184 Box__string = Box__string{
        value: "generic",
    }
    var t185 string
    var inline221 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t184)
    t185 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline218)
    var t187 int32
    var inline216 int32 = 11
    t187 = inline216
    var t188 string
    var inline214 string = _goml_runtime_core_int32_to_string(t187)
    t188 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t209 string = self__1.value
    return t209
}

func main() {
    main0()
}
