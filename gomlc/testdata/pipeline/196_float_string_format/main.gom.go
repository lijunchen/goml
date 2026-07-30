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
    var t75 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(18318654708.7)
    _goml_runtime_core_string_println(t75)
    var t76 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(0.0000001)
    _goml_runtime_core_string_println(t76)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t77 float64 = negative_one__1 * zero__0
    var t78 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t77)
    _goml_runtime_core_string_println(t78)
    var t79 float64 = 1 / zero__0
    var t80 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t79)
    _goml_runtime_core_string_println(t80)
    var t81 float64 = -1
    var t82 float64 = t81 / zero__0
    var t83 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t82)
    _goml_runtime_core_string_println(t83)
    var t84 float64 = zero__0 / zero__0
    var t85 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t84)
    _goml_runtime_core_string_println(t85)
    var wide__2 float64 = 12345678
    var t86 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(wide__2)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv89 string
    var t90 string = _goml_runtime_core_float64_to_string(self__50)
    retv89 = t90
    return retv89
}

func main() {
    main0()
}
