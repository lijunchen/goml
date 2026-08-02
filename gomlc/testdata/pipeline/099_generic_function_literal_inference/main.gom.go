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
    var t159 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t159)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t160 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t160)
    var c__3 int64 = identity__T_int64(100)
    var t161 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t161)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    return x__0
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t169 string = _goml_runtime_core_uint8_to_string(self__45)
    return t169
}

func identity__T_float32(x__0 float32) float32 {
    return x__0
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var t174 string = _goml_runtime_core_float32_to_string(self__49)
    return t174
}

func identity__T_int64(x__0 int64) int64 {
    return x__0
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var t179 string = _goml_runtime_core_int64_to_string(self__44)
    return t179
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
