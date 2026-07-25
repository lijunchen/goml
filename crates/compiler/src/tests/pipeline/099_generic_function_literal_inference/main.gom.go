package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__1 uint8 = identity__T_uint8(42)
    var t68 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t68)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t69 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t69)
    var c__3 int64 = identity__T_int64(100)
    var t70 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t70)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv72 uint8
    retv72 = x__0
    return retv72
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv77 string
    var t78 string = _goml_runtime_core_uint8_to_string(self__45)
    retv77 = t78
    return retv77
}

func identity__T_float32(x__0 float32) float32 {
    var retv80 float32
    retv80 = x__0
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_float32_to_string(self__49)
    retv82 = t83
    return retv82
}

func identity__T_int64(x__0 int64) int64 {
    var retv85 int64
    retv85 = x__0
    return retv85
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int64_to_string(self__44)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv90 string
    retv90 = self__38
    return retv90
}

func main() {
    main0()
}
