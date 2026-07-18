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
    var t65 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__1)
    println__T_string(t65)
    var b__2 float32 = identity__T_float32(3.140000104904175)
    var t66 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(b__2)
    println__T_string(t66)
    var c__3 int64 = identity__T_int64(100)
    var t67 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(c__3)
    println__T_string(t67)
    return struct{}{}
}

func identity__T_uint8(x__0 uint8) uint8 {
    var retv69 uint8
    retv69 = x__0
    return retv69
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__43 uint8) string {
    var retv74 string
    var t75 string = _goml_runtime_core_uint8_to_string(self__43)
    retv74 = t75
    return retv74
}

func identity__T_float32(x__0 float32) float32 {
    var retv77 float32
    retv77 = x__0
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__47 float32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_float32_to_string(self__47)
    retv79 = t80
    return retv79
}

func identity__T_int64(x__0 int64) int64 {
    var retv82 int64
    retv82 = x__0
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__42 int64) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int64_to_string(self__42)
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv87 string
    retv87 = self__37
    return retv87
}

func main() {
    main0()
}
