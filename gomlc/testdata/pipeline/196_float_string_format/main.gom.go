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
    var t194 string
    var inline250 float64 = 18318654708.7
    var inline251 string = _goml_runtime_core_float64_to_string(inline250)
    t194 = inline251
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline247)
    var t195 string
    var inline244 float64 = 0.0000001
    var inline245 string = _goml_runtime_core_float64_to_string(inline244)
    t195 = inline245
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline241)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t196 float64 = negative_one__1 * zero__0
    var t197 string
    var inline239 string = _goml_runtime_core_float64_to_string(t196)
    t197 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline236)
    var t198 float64 = 1 / zero__0
    var t199 string
    var inline234 string = _goml_runtime_core_float64_to_string(t198)
    t199 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline231)
    var t200 float64 = -1
    var t201 float64 = t200 / zero__0
    var t202 string
    var inline229 string = _goml_runtime_core_float64_to_string(t201)
    t202 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline226)
    var t203 float64 = zero__0 / zero__0
    var t204 string
    var inline224 string = _goml_runtime_core_float64_to_string(t203)
    t204 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline221)
    var wide__2 float64 = 12345678
    var t205 string
    var inline219 string = _goml_runtime_core_float64_to_string(wide__2)
    t205 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
