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

func increment(value__0 uint8) uint8 {
    var retv76 uint8
    var t77 uint8 = value__0 + 1
    retv76 = t77
    return retv76
}

func one_float32() float32 {
    var retv79 float32
    retv79 = 1
    return retv79
}

func two_int16() int16 {
    var retv81 int16
    retv81 = 2
    return retv81
}

func read_uint32(value__1 *ref_uint32_x) uint32 {
    var retv83 uint32
    var t84 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(value__1)
    retv83 = t84
    return retv83
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp87 uint8
    if true {
        jp87 = 1
    } else {
        jp87 = small__4
    }
    var branch__6 uint8 = jp87
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t88 int = default_integer__2 + 2
    var t89 string = _goml_m_inherent_i_int_i_int_i_to__string(t88)
    println__T_string(t89)
    var t90 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t90)
    var t91 uint8 = increment(small__4)
    var t92 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t91)
    println__T_string(t92)
    var t93 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t94 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t93)
    println__T_string(t94)
    var t95 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(branch__6)
    println__T_string(t95)
    var t96 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t97 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(t96)
    println__T_string(t97)
    var t98 float32 = one_float32()
    var t99 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t98)
    println__T_string(t99)
    var t100 int16 = two_int16()
    var t101 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t100)
    println__T_string(t101)
    var t102 uint32 = read_uint32(delayed__8)
    var t103 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t102)
    println__T_string(t103)
    var t104 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__9, 65)
    var t105 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t104)
    println__T_string(t105)
    var jp107 string
    switch byte__9 {
    case 65:
        jp107 = "byte"
    default:
        jp107 = "other"
    }
    println__T_string(jp107)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__210 *ref_uint32_x) uint32 {
    var retv109 uint32
    var t110 uint32 = ref_get__Ref_6uint32(self__210)
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__209 uint32) *ref_uint32_x {
    var retv112 *ref_uint32_x
    var t113 *ref_uint32_x = ref__Ref_6uint32(value__209)
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv115 uint8
    var t116 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv115 = t116
    return retv115
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int_to_string(self__5)
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv124 string
    var t125 string = _goml_runtime_core_float64_to_string(self__50)
    retv124 = t125
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv127 string
    var t128 string = _goml_runtime_core_uint8_to_string(self__45)
    retv127 = t128
    return retv127
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv130 string
    var t131 string = _goml_runtime_core_uint16_to_string(self__46)
    retv130 = t131
    return retv130
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv133 string
    var t134 string = _goml_runtime_core_float32_to_string(self__49)
    retv133 = t134
    return retv133
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv136 string
    var t137 string = _goml_runtime_core_int16_to_string(self__42)
    retv136 = t137
    return retv136
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv139 string
    var t140 string = _goml_runtime_core_uint32_to_string(self__47)
    retv139 = t140
    return retv139
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv142 bool
    var t143 bool = self__69 == other__70
    retv142 = t143
    return retv142
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv145 string
    var t146 string = _goml_runtime_core_bool_to_string(self__37)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv148 string
    retv148 = self__38
    return retv148
}

func main() {
    main0()
}
