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
    var t424 uint8 = value__0 + 1
    return t424
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp434 uint8
    jp434 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__u32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t435 int = default_integer__2 + 2
    var t436 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t435)
    println__T_string(t436)
    var t437 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(default_float__3)
    println__T_string(t437)
    var t438 uint8 = increment(small__4)
    var t439 string
    var inline547 string = _goml_runtime_core_uint8_to_string(t438)
    t439 = inline547
    var inline544 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline544)
    var t440 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t441 string
    var inline542 string = _goml_runtime_core_uint8_to_string(t440)
    t441 = inline542
    var inline539 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline539)
    var t442 string
    var inline537 string = _goml_runtime_core_uint8_to_string(jp434)
    t442 = inline537
    var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t442)
    _goml_runtime_core_string_println(inline534)
    var t443 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t444 string
    var inline532 string = _goml_runtime_core_uint16_to_string(t443)
    t444 = inline532
    var inline529 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
    _goml_runtime_core_string_println(inline529)
    var t445 float32
    t445 = 1
    var t446 string
    var inline526 string = _goml_runtime_core_float32_to_string(t445)
    t446 = inline526
    var inline523 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline523)
    var t447 int16
    t447 = 2
    var t448 string
    var inline520 string = _goml_runtime_core_int16_to_string(t447)
    t448 = inline520
    var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline517)
    var t449 uint32
    var inline515 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__u32(delayed__8)
    t449 = inline515
    var t450 string
    var inline513 string = _goml_runtime_core_uint32_to_string(t449)
    t450 = inline513
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline510)
    var t451 bool = byte__9 == 65
    var t452 string
    var inline508 string = _goml_runtime_core_bool_to_string(t451)
    t452 = inline508
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
    _goml_runtime_core_string_println(inline505)
    var jp454 string
    switch byte__9 {
    case 65:
        jp454 = "byte"
    default:
        jp454 = "other"
    }
    var inline502 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp454)
    _goml_runtime_core_string_println(inline502)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__u32(self__432 *ref_uint32_x) uint32 {
    var t457 uint32 = ref_get__Ref_6uint32(self__432)
    return t457
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__u32(value__431 uint32) *ref_uint32_x {
    var t460 *ref_uint32_x = ref__Ref_6uint32(value__431)
    return t460
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t463 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t463
}

func println__T_string(value__1 string) struct{} {
    var t465 string
    t465 = value__1
    _goml_runtime_core_string_println(t465)
    return struct{}{}
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t469 string = _goml_runtime_core_int_to_string(self__32)
    return t469
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__161 float64) string {
    var t472 string = _goml_runtime_core_float64_to_string(self__161)
    return t472
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
