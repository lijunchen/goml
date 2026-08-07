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
    var t179 string
    var inline235 float64 = 18318654708.7
    var inline236 string = _goml_runtime_core_float64_to_string(inline235)
    t179 = inline236
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline232)
    var t180 string
    var inline229 float64 = 0.0000001
    var inline230 string = _goml_runtime_core_float64_to_string(inline229)
    t180 = inline230
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline226)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t181 float64 = negative_one__1 * zero__0
    var t182 string
    var inline224 string = _goml_runtime_core_float64_to_string(t181)
    t182 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline221)
    var t183 float64 = 1 / zero__0
    var t184 string
    var inline219 string = _goml_runtime_core_float64_to_string(t183)
    t184 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline216)
    var t185 float64 = -1
    var t186 float64 = t185 / zero__0
    var t187 string
    var inline214 string = _goml_runtime_core_float64_to_string(t186)
    t187 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline211)
    var t188 float64 = zero__0 / zero__0
    var t189 string
    var inline209 string = _goml_runtime_core_float64_to_string(t188)
    t189 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline206)
    var wide__2 float64 = 12345678
    var t190 string
    var inline204 string = _goml_runtime_core_float64_to_string(wide__2)
    t190 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
