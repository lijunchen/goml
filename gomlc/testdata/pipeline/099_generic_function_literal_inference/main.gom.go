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
    var t156 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t156)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t157 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t157)
    var c__3 int64 = identity__T_int64(100)
    var t158 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t158)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv160 uint8
    retv160 = x__0
    return retv160
}

func println__T_string(value__1 string) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv165 string
    var t166 string = _goml_runtime_core_uint8_to_string(self__45)
    retv165 = t166
    return retv165
}

func identity__T_float32(x__0 float32) float32 {
    var retv168 float32
    retv168 = x__0
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_float32_to_string(self__49)
    retv170 = t171
    return retv170
}

func identity__T_int64(x__0 int64) int64 {
    var retv173 int64
    retv173 = x__0
    return retv173
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int64_to_string(self__44)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv178 string
    retv178 = self__38
    return retv178
}

func main() {
    main0()
}
