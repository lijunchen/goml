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
    var t189 string
    var inline245 float64 = 18318654708.7
    var inline246 string = _goml_runtime_core_float64_to_string(inline245)
    t189 = inline246
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline242)
    var t190 string
    var inline239 float64 = 0.0000001
    var inline240 string = _goml_runtime_core_float64_to_string(inline239)
    t190 = inline240
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline236)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t191 float64 = negative_one__1 * zero__0
    var t192 string
    var inline234 string = _goml_runtime_core_float64_to_string(t191)
    t192 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline231)
    var t193 float64 = 1 / zero__0
    var t194 string
    var inline229 string = _goml_runtime_core_float64_to_string(t193)
    t194 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline226)
    var t195 float64 = -1
    var t196 float64 = t195 / zero__0
    var t197 string
    var inline224 string = _goml_runtime_core_float64_to_string(t196)
    t197 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline221)
    var t198 float64 = zero__0 / zero__0
    var t199 string
    var inline219 string = _goml_runtime_core_float64_to_string(t198)
    t199 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline216)
    var wide__2 float64 = 12345678
    var t200 string
    var inline214 string = _goml_runtime_core_float64_to_string(wide__2)
    t200 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
