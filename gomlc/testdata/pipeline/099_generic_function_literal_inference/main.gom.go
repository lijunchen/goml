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
    var inline224 uint8 = 42
    a__1 = inline224
    var t181 string
    var inline222 string = _goml_runtime_core_uint8_to_string(a__1)
    t181 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline219)
    var b__2 float32
    var inline217 float32 = 3.140000104904175
    b__2 = inline217
    var t182 string
    var inline215 string = _goml_runtime_core_float32_to_string(b__2)
    t182 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline212)
    var c__3 int64
    var inline210 int64 = 100
    c__3 = inline210
    var t183 string
    var inline208 string = _goml_runtime_core_int64_to_string(c__3)
    t183 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
