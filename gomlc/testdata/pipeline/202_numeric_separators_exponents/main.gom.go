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
    var inline244 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(integer__1)
    _goml_runtime_core_string_println(inline244)
    var inline241 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(unsigned__2)
    _goml_runtime_core_string_println(inline241)
    var inline238 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(float__3)
    _goml_runtime_core_string_println(inline238)
    var inline235 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(small__4)
    _goml_runtime_core_string_println(inline235)
    switch integer__1 {
    case 1000:
        var inline227 string = "matched"
        var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline227)
        _goml_runtime_core_string_println(inline228)
        return struct{}{}
    default:
        var inline231 string = "missed"
        var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline231)
        _goml_runtime_core_string_println(inline232)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t214 string = _goml_runtime_core_int32_to_string(self__70)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__75 uint64) string {
    var t217 string = _goml_runtime_core_uint64_to_string(self__75)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t220 string = _goml_runtime_core_float64_to_string(self__77)
    return t220
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__76 float32) string {
    var t223 string = _goml_runtime_core_float32_to_string(self__76)
    return t223
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
