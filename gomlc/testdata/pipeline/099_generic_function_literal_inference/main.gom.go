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
    var inline229 uint8 = 42
    a__1 = inline229
    var t186 string
    var inline227 string = _goml_runtime_core_uint8_to_string(a__1)
    t186 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline224)
    var b__2 float32
    var inline222 float32 = 3.140000104904175
    b__2 = inline222
    var t187 string
    var inline220 string = _goml_runtime_core_float32_to_string(b__2)
    t187 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline217)
    var c__3 int64
    var inline215 int64 = 100
    c__3 = inline215
    var t188 string
    var inline213 string = _goml_runtime_core_int64_to_string(c__3)
    t188 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
