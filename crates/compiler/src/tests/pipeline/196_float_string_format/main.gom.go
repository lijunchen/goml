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
    var t71 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(18318654708.7)
    _goml_runtime_core_string_println(t71)
    var t72 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(0.0000001)
    _goml_runtime_core_string_println(t72)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t73 float64 = negative_one__1 * zero__0
    var t74 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t73)
    _goml_runtime_core_string_println(t74)
    var t75 float64 = 1 / zero__0
    var t76 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t75)
    _goml_runtime_core_string_println(t76)
    var t77 float64 = -1
    var t78 float64 = t77 / zero__0
    var t79 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t78)
    _goml_runtime_core_string_println(t79)
    var t80 float64 = zero__0 / zero__0
    var t81 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t80)
    _goml_runtime_core_string_println(t81)
    var wide__2 float64 = 12345678
    var t82 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(wide__2)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv85 string
    var t86 string = _goml_runtime_core_float64_to_string(self__50)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
