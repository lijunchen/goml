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
    var t79 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func println__T_float64(value__1 float64) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func println__T_float32(value__1 float32) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__43)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv97 string
    var t98 string = _goml_runtime_core_uint64_to_string(self__48)
    retv97 = t98
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv100 string
    var t101 string = _goml_runtime_core_float64_to_string(self__50)
    retv100 = t101
    return retv100
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv103 string
    var t104 string = _goml_runtime_core_float32_to_string(self__49)
    retv103 = t104
    return retv103
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv106 string
    retv106 = self__38
    return retv106
}

func main() {
    main0()
}
