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
    var t112 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t112)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t113 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t113)
    var c__3 int64 = identity__T_int64(100)
    var t114 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t114)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv116 uint8
    retv116 = x__0
    return retv116
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv121 string
    var t122 string = _goml_runtime_core_uint8_to_string(self__45)
    retv121 = t122
    return retv121
}

func identity__T_float32(x__0 float32) float32 {
    var retv124 float32
    retv124 = x__0
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_float32_to_string(self__49)
    retv126 = t127
    return retv126
}

func identity__T_int64(x__0 int64) int64 {
    var retv129 int64
    retv129 = x__0
    return retv129
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv131 string
    var t132 string = _goml_runtime_core_int64_to_string(self__44)
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv134 string
    retv134 = self__38
    return retv134
}

func main() {
    main0()
}
