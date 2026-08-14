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

type Ordering int32

func main0() struct{} {
    var a__1 uint8
    var inline455 uint8 = 42
    a__1 = inline455
    var t412 string
    var inline453 string = _goml_runtime_core_uint8_to_string(a__1)
    t412 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t412)
    _goml_runtime_core_string_println(inline450)
    var b__2 float32
    var inline448 float32 = 3.140000104904175
    b__2 = inline448
    var t413 string
    var inline446 string = _goml_runtime_core_float32_to_string(b__2)
    t413 = inline446
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t413)
    _goml_runtime_core_string_println(inline443)
    var c__3 int64
    var inline441 int64 = 100
    c__3 = inline441
    var t414 string
    var inline439 string = _goml_runtime_core_int64_to_string(c__3)
    t414 = inline439
    var inline436 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t414)
    _goml_runtime_core_string_println(inline436)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
