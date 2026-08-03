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
    var t141 int32 = self__0.value
    return t141
}

func main0() struct{} {
    var t143 Number = Number{
        value: 42,
    }
    var t144 int32
    var inline199 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t143)
    t144 = inline199
    var t145 string
    var inline197 string = _goml_runtime_core_int32_to_string(t144)
    t145 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline194)
    var t146 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline192 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t146)
    value__4 = inline192
    var t147 string
    var inline190 string = _goml_runtime_core_int32_to_string(value__4)
    t147 = inline190
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline187)
    var t148 Box__string = Box__string{
        value: "generic",
    }
    var t149 string
    var inline185 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t148)
    t149 = inline185
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t149)
    _goml_runtime_core_string_println(inline182)
    var t151 int32
    var inline180 int32 = 11
    t151 = inline180
    var t152 string
    var inline178 string = _goml_runtime_core_int32_to_string(t151)
    t152 = inline178
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
    _goml_runtime_core_string_println(inline175)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t173 string = self__1.value
    return t173
}

func main() {
    main0()
}
