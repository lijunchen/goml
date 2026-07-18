package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_float32_to_string(x float32) string {
    var formatted string = _goml_strconv.FormatFloat(float64(x), 102, -1, 32)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

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
    var t68 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(18318654708.7)
    _goml_runtime_core_string_println(t68)
    var t69 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(0.0000001)
    _goml_runtime_core_string_println(t69)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t70 float64 = negative_one__1 * zero__0
    var t71 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t70)
    _goml_runtime_core_string_println(t71)
    var t72 float64 = 1 / zero__0
    var t73 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t72)
    _goml_runtime_core_string_println(t73)
    var t74 float64 = -1
    var t75 float64 = t74 / zero__0
    var t76 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t75)
    _goml_runtime_core_string_println(t76)
    var t77 float64 = zero__0 / zero__0
    var t78 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(t77)
    _goml_runtime_core_string_println(t78)
    var wide__2 float32 = 12345678
    var t79 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(wide__2)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__48 float64) string {
    var retv82 string
    var t83 string = _goml_runtime_core_float64_to_string(self__48)
    retv82 = t83
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__47 float32) string {
    var retv85 string
    var t86 string = _goml_runtime_core_float32_to_string(self__47)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
