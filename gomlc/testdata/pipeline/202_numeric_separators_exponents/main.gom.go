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
    var t119 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func println__T_float64(value__1 float64) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func println__T_float32(value__1 float32) struct{} {
    var t128 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t131 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t131)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int32_to_string(self__43)
    retv134 = t135
    return retv134
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv137 string
    var t138 string = _goml_runtime_core_uint64_to_string(self__48)
    retv137 = t138
    return retv137
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv140 string
    var t141 string = _goml_runtime_core_float64_to_string(self__50)
    retv140 = t141
    return retv140
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv143 string
    var t144 string = _goml_runtime_core_float32_to_string(self__49)
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv146 string
    retv146 = self__38
    return retv146
}

func main() {
    main0()
}
