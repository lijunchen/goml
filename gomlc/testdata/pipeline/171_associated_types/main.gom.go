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
    var t160 int32 = self__0.value
    return t160
}

func main0() struct{} {
    var t162 Number = Number{
        value: 42,
    }
    var t163 int32
    var inline218 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t162)
    t163 = inline218
    var t164 string
    var inline216 string = _goml_runtime_core_int32_to_string(t163)
    t164 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline213)
    var t165 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline211 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t165)
    value__4 = inline211
    var t166 string
    var inline209 string = _goml_runtime_core_int32_to_string(value__4)
    t166 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline206)
    var t167 Box__string = Box__string{
        value: "generic",
    }
    var t168 string
    var inline204 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t167)
    t168 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t168)
    _goml_runtime_core_string_println(inline201)
    var t170 int32
    var inline199 int32 = 11
    t170 = inline199
    var t171 string
    var inline197 string = _goml_runtime_core_int32_to_string(t170)
    t171 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t192 string = self__1.value
    return t192
}

func main() {
    main0()
}
