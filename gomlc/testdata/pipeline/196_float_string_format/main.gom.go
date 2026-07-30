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
    var t115 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(18318654708.7)
    _goml_runtime_core_string_println(t115)
    var t116 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(0.0000001)
    _goml_runtime_core_string_println(t116)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t117 float64 = negative_one__1 * zero__0
    var t118 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t117)
    _goml_runtime_core_string_println(t118)
    var t119 float64 = 1 / zero__0
    var t120 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t119)
    _goml_runtime_core_string_println(t120)
    var t121 float64 = -1
    var t122 float64 = t121 / zero__0
    var t123 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t122)
    _goml_runtime_core_string_println(t123)
    var t124 float64 = zero__0 / zero__0
    var t125 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t124)
    _goml_runtime_core_string_println(t125)
    var wide__2 float64 = 12345678
    var t126 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(wide__2)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv129 string
    var t130 string = _goml_runtime_core_float64_to_string(self__50)
    retv129 = t130
    return retv129
}

func main() {
    main0()
}
