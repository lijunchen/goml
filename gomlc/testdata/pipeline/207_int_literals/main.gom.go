package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
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

func array_get__Array_2_5uint8(arr [2]uint8, index int) uint8 {
    return arr[index]
}

func array_get__Array_2_6uint16(arr [2]uint16, index int) uint16 {
    return arr[index]
}

type ref_uint32_x struct {
    value uint32
}

func ref__Ref_6uint32(value uint32) *ref_uint32_x {
    return &ref_uint32_x{
        value: value,
    }
}

func ref_get__Ref_6uint32(reference *ref_uint32_x) uint32 {
    return reference.value
}

type Ordering int32

func increment(value__0 uint8) uint8 {
    var t421 uint8 = value__0 + 1
    return t421
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp431 uint8
    jp431 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t432 int = default_integer__2 + 2
    var t433 string = _goml_m_inherent_i_int_i_int_i_to__string(t432)
    println__T_string(t433)
    var t434 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t434)
    var t435 uint8 = increment(small__4)
    var t436 string
    var inline544 string = _goml_runtime_core_uint8_to_string(t435)
    t436 = inline544
    var inline541 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline541)
    var t437 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t438 string
    var inline539 string = _goml_runtime_core_uint8_to_string(t437)
    t438 = inline539
    var inline536 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline536)
    var t439 string
    var inline534 string = _goml_runtime_core_uint8_to_string(jp431)
    t439 = inline534
    var inline531 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline531)
    var t440 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t441 string
    var inline529 string = _goml_runtime_core_uint16_to_string(t440)
    t441 = inline529
    var inline526 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline526)
    var t442 float32
    t442 = 1
    var t443 string
    var inline523 string = _goml_runtime_core_float32_to_string(t442)
    t443 = inline523
    var inline520 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline520)
    var t444 int16
    t444 = 2
    var t445 string
    var inline517 string = _goml_runtime_core_int16_to_string(t444)
    t445 = inline517
    var inline514 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline514)
    var t446 uint32
    var inline512 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t446 = inline512
    var t447 string
    var inline510 string = _goml_runtime_core_uint32_to_string(t446)
    t447 = inline510
    var inline507 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline507)
    var t448 bool = byte__9 == 65
    var t449 string
    var inline505 string = _goml_runtime_core_bool_to_string(t448)
    t449 = inline505
    var inline502 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline502)
    var jp451 string
    switch byte__9 {
    case 65:
        jp451 = "byte"
    default:
        jp451 = "other"
    }
    var inline499 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp451)
    _goml_runtime_core_string_println(inline499)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__432 *ref_uint32_x) uint32 {
    var t454 uint32 = ref_get__Ref_6uint32(self__432)
    return t454
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__431 uint32) *ref_uint32_x {
    var t457 *ref_uint32_x = ref__Ref_6uint32(value__431)
    return t457
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t460 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t460
}

func println__T_string(value__1 string) struct{} {
    var t462 string
    t462 = value__1
    _goml_runtime_core_string_println(t462)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t466 string = _goml_runtime_core_int_to_string(self__32)
    return t466
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__161 float64) string {
    var t469 string = _goml_runtime_core_float64_to_string(self__161)
    return t469
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
