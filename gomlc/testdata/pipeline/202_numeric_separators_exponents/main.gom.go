package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
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
    var integer__1 int32 = 1000
    var unsigned__2 uint64 = 4294967296
    var float__3 float64 = 125
    var small__4 float32 = 2.5
    println__T_int32(integer__1)
    println__T_uint64(unsigned__2)
    println__T_float64(float__3)
    println__T_float32(small__4)
    switch integer__1 {
    case 1000:
        println__T_string("matched")
    default:
        println__T_string("missed")
    }
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func println__T_float64(value__1 float64) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func println__T_float32(value__1 float32) struct{} {
    var t175 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    _goml_runtime_core_string_println(t175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t178)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv181 string
    var t182 string = _goml_runtime_core_int32_to_string(self__43)
    retv181 = t182
    return retv181
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv184 string
    var t185 string = _goml_runtime_core_uint64_to_string(self__48)
    retv184 = t185
    return retv184
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv187 string
    var t188 string = _goml_runtime_core_float64_to_string(self__50)
    retv187 = t188
    return retv187
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv190 string
    var t191 string = _goml_runtime_core_float32_to_string(self__49)
    retv190 = t191
    return retv190
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv193 string
    retv193 = self__38
    return retv193
}

func main() {
    main0()
}
