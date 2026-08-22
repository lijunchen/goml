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

type Ordering int32

func main0() struct{} {
    var integer__1 int32 = 1000
    var unsigned__2 uint64 = 4294967296
    var float__3 float64 = 125
    var small__4 float32 = 2.5
    var inline468 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(integer__1)
    _goml_runtime_core_string_println(inline468)
    var inline465 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(unsigned__2)
    _goml_runtime_core_string_println(inline465)
    var inline462 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(float__3)
    _goml_runtime_core_string_println(inline462)
    var inline459 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(small__4)
    _goml_runtime_core_string_println(inline459)
    switch integer__1 {
    case 1000:
        var inline451 string = "matched"
        var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline451)
        _goml_runtime_core_string_println(inline452)
        return struct{}{}
    default:
        var inline455 string = "missed"
        var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline455)
        _goml_runtime_core_string_println(inline456)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t438 string = _goml_runtime_core_int32_to_string(self__154)
    return t438
}

func _goml_m_trait__impl_i_ToString_i_u64_i_to__string(self__159 uint64) string {
    var t441 string = _goml_runtime_core_uint64_to_string(self__159)
    return t441
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__161 float64) string {
    var t444 string = _goml_runtime_core_float64_to_string(self__161)
    return t444
}

func _goml_m_trait__impl_i_ToString_i_f32_i_to__string(self__160 float32) string {
    var t447 string = _goml_runtime_core_float32_to_string(self__160)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
