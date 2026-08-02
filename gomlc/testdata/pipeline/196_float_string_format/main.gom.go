package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_float64_to_string(x float64) string {
    var formatted string = _goml_strconv.FormatFloat(x, 102, -1, 64)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t162 string
    var inline218 float64 = 18318654708.7
    var inline219 string = _goml_runtime_core_float64_to_string(inline218)
    t162 = inline219
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
    _goml_runtime_core_string_println(inline215)
    var t163 string
    var inline212 float64 = 0.0000001
    var inline213 string = _goml_runtime_core_float64_to_string(inline212)
    t163 = inline213
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline209)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t164 float64 = negative_one__1 * zero__0
    var t165 string
    var inline207 string = _goml_runtime_core_float64_to_string(t164)
    t165 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline204)
    var t166 float64 = 1 / zero__0
    var t167 string
    var inline202 string = _goml_runtime_core_float64_to_string(t166)
    t167 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline199)
    var t168 float64 = -1
    var t169 float64 = t168 / zero__0
    var t170 string
    var inline197 string = _goml_runtime_core_float64_to_string(t169)
    t170 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline194)
    var t171 float64 = zero__0 / zero__0
    var t172 string
    var inline192 string = _goml_runtime_core_float64_to_string(t171)
    t172 = inline192
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline189)
    var wide__2 float64 = 12345678
    var t173 string
    var inline187 string = _goml_runtime_core_float64_to_string(wide__2)
    t173 = inline187
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
