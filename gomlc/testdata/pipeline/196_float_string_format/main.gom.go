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
    var t159 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(18318654708.7)
    _goml_runtime_core_string_println(t159)
    var t160 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(0.0000001)
    _goml_runtime_core_string_println(t160)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t161 float64 = negative_one__1 * zero__0
    var t162 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t161)
    _goml_runtime_core_string_println(t162)
    var t163 float64 = 1 / zero__0
    var t164 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t163)
    _goml_runtime_core_string_println(t164)
    var t165 float64 = -1
    var t166 float64 = t165 / zero__0
    var t167 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t166)
    _goml_runtime_core_string_println(t167)
    var t168 float64 = zero__0 / zero__0
    var t169 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t168)
    _goml_runtime_core_string_println(t169)
    var wide__2 float64 = 12345678
    var t170 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(wide__2)
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv173 string
    var t174 string = _goml_runtime_core_float64_to_string(self__50)
    retv173 = t174
    return retv173
}

func main() {
    main0()
}
