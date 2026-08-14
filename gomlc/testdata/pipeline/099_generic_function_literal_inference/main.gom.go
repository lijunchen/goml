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
    var inline234 uint8 = 42
    a__1 = inline234
    var t191 string
    var inline232 string = _goml_runtime_core_uint8_to_string(a__1)
    t191 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline229)
    var b__2 float32
    var inline227 float32 = 3.140000104904175
    b__2 = inline227
    var t192 string
    var inline225 string = _goml_runtime_core_float32_to_string(b__2)
    t192 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline222)
    var c__3 int64
    var inline220 int64 = 100
    c__3 = inline220
    var t193 string
    var inline218 string = _goml_runtime_core_int64_to_string(c__3)
    t193 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
