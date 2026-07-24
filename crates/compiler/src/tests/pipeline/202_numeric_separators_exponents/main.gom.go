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
    var t72 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func println__T_float64(value__1 float64) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func println__T_float32(value__1 float32) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__41)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__46 uint64) string {
    var retv90 string
    var t91 string = _goml_runtime_core_uint64_to_string(self__46)
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__48 float64) string {
    var retv93 string
    var t94 string = _goml_runtime_core_float64_to_string(self__48)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__47 float32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_float32_to_string(self__47)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv99 string
    retv99 = self__37
    return retv99
}

func main() {
    main0()
}
