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
    var inline234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(integer__1)
    _goml_runtime_core_string_println(inline234)
    var inline231 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(unsigned__2)
    _goml_runtime_core_string_println(inline231)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(float__3)
    _goml_runtime_core_string_println(inline228)
    var inline225 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(small__4)
    _goml_runtime_core_string_println(inline225)
    switch integer__1 {
    case 1000:
        var inline217 string = "matched"
        var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline217)
        _goml_runtime_core_string_println(inline218)
        return struct{}{}
    default:
        var inline221 string = "missed"
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline221)
        _goml_runtime_core_string_println(inline222)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__72)
    return t204
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t207 string = _goml_runtime_core_uint64_to_string(self__77)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t210 string = _goml_runtime_core_float64_to_string(self__79)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__78 float32) string {
    var t213 string = _goml_runtime_core_float32_to_string(self__78)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
