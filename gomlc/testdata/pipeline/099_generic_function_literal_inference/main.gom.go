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
    var a__1 uint8
    var inline219 uint8 = 42
    a__1 = inline219
    var t176 string
    var inline217 string = _goml_runtime_core_uint8_to_string(a__1)
    t176 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline214)
    var b__2 float32
    var inline212 float32 = 3.140000104904175
    b__2 = inline212
    var t177 string
    var inline210 string = _goml_runtime_core_float32_to_string(b__2)
    t177 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline207)
    var c__3 int64
    var inline205 int64 = 100
    c__3 = inline205
    var t178 string
    var inline203 string = _goml_runtime_core_int64_to_string(c__3)
    t178 = inline203
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
