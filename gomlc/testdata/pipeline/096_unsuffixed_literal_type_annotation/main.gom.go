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
    var t423 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t423)
    var t424 string
    var inline512 string = _goml_runtime_core_int8_to_string(b__3)
    t424 = inline512
    var inline509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline509)
    var t425 string
    var inline507 string = _goml_runtime_core_int16_to_string(c__4)
    t425 = inline507
    var inline504 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline504)
    var t426 string
    var inline502 string = _goml_runtime_core_uint16_to_string(d__5)
    t426 = inline502
    var inline499 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline499)
    var t427 string
    var inline497 string = _goml_runtime_core_uint32_to_string(e__6)
    t427 = inline497
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline494)
    var t428 string
    var inline492 string = _goml_runtime_core_int64_to_string(f__7)
    t428 = inline492
    var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline489)
    var t429 string
    var inline487 string = _goml_runtime_core_uint64_to_string(g__8)
    t429 = inline487
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline484)
    var t430 string
    var inline482 string = _goml_runtime_core_float32_to_string(h__9)
    t430 = inline482
    var inline479 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline479)
    var t431 uint8
    var inline477 uint8 = 10
    t431 = inline477
    var t432 string
    var inline475 string = _goml_runtime_core_uint8_to_string(t431)
    t432 = inline475
    var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline472)
    var t433 float32
    var inline470 float32 = 2.5
    t433 = inline470
    var t434 string
    var inline468 string = _goml_runtime_core_float32_to_string(t433)
    t434 = inline468
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline465)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t436 string
    t436 = value__1
    _goml_runtime_core_string_println(t436)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__156 uint8) string {
    var t440 string = _goml_runtime_core_uint8_to_string(self__156)
    return t440
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
