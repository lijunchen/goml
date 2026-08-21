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
    var inline458 uint8 = 42
    a__1 = inline458
    var t415 string
    var inline456 string = _goml_runtime_core_uint8_to_string(a__1)
    t415 = inline456
    var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline453)
    var b__2 float32
    var inline451 float32 = 3.140000104904175
    b__2 = inline451
    var t416 string
    var inline449 string = _goml_runtime_core_float32_to_string(b__2)
    t416 = inline449
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline446)
    var c__3 int64
    var inline444 int64 = 100
    c__3 = inline444
    var t417 string
    var inline442 string = _goml_runtime_core_int64_to_string(c__3)
    t417 = inline442
    var inline439 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline439)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
