package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var a__2 uint8 = 1
    var b__3 int8 = 2
    var c__4 int16 = 3
    var d__5 uint16 = 4
    var e__6 uint32 = 5
    var f__7 int64 = 6
    var g__8 uint64 = 7
    var h__9 float32 = 1
    var t426 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(a__2)
    println__T_string(t426)
    var t427 string
    var inline515 string = _goml_runtime_core_int8_to_string(b__3)
    t427 = inline515
    var inline512 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline512)
    var t428 string
    var inline510 string = _goml_runtime_core_int16_to_string(c__4)
    t428 = inline510
    var inline507 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline507)
    var t429 string
    var inline505 string = _goml_runtime_core_uint16_to_string(d__5)
    t429 = inline505
    var inline502 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline502)
    var t430 string
    var inline500 string = _goml_runtime_core_uint32_to_string(e__6)
    t430 = inline500
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline497)
    var t431 string
    var inline495 string = _goml_runtime_core_int64_to_string(f__7)
    t431 = inline495
    var inline492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline492)
    var t432 string
    var inline490 string = _goml_runtime_core_uint64_to_string(g__8)
    t432 = inline490
    var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline487)
    var t433 string
    var inline485 string = _goml_runtime_core_float32_to_string(h__9)
    t433 = inline485
    var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline482)
    var t434 uint8
    var inline480 uint8 = 10
    t434 = inline480
    var t435 string
    var inline478 string = _goml_runtime_core_uint8_to_string(t434)
    t435 = inline478
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline475)
    var t436 float32
    var inline473 float32 = 2.5
    t436 = inline473
    var t437 string
    var inline471 string = _goml_runtime_core_float32_to_string(t436)
    t437 = inline471
    var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline468)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t439 string
    t439 = value__1
    _goml_runtime_core_string_println(t439)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_u8_i_to__string(self__156 uint8) string {
    var t443 string = _goml_runtime_core_uint8_to_string(self__156)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
