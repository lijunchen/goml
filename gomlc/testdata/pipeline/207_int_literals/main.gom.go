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
    var t168 uint8 = value__0 + 1
    return t168
}

func one_float32() float32 {
    return 1
}

func two_int16() int16 {
    return 2
}

func read_uint32(value__1 *ref_uint32_x) uint32 {
    var t175 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(value__1)
    return t175
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp178 uint8
    jp178 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t179 int = default_integer__2 + 2
    var t180 string = _goml_m_inherent_i_int_i_int_i_to__string(t179)
    println__T_string(t180)
    var t181 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t181)
    var t182 uint8 = increment(small__4)
    var t183 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t182)
    println__T_string(t183)
    var t184 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t185 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t184)
    println__T_string(t185)
    var t186 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(jp178)
    println__T_string(t186)
    var t187 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t188 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(t187)
    println__T_string(t188)
    var t189 float32 = one_float32()
    var t190 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t189)
    println__T_string(t190)
    var t191 int16 = two_int16()
    var t192 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t191)
    println__T_string(t192)
    var t193 uint32 = read_uint32(delayed__8)
    var t194 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t193)
    println__T_string(t194)
    var t195 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__9, 65)
    var t196 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t195)
    println__T_string(t196)
    var jp198 string
    switch byte__9 {
    case 65:
        jp198 = "byte"
    default:
        jp198 = "other"
    }
    println__T_string(jp198)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__208 *ref_uint32_x) uint32 {
    var t201 uint32 = ref_get__Ref_6uint32(self__208)
    return t201
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__207 uint32) *ref_uint32_x {
    var t204 *ref_uint32_x = ref__Ref_6uint32(value__207)
    return t204
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var t207 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    return t207
}

func println__T_string(value__1 string) struct{} {
    var t209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t213 string = _goml_runtime_core_int_to_string(self__5)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var t216 string = _goml_runtime_core_float64_to_string(self__50)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t219 string = _goml_runtime_core_uint8_to_string(self__45)
    return t219
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var t222 string = _goml_runtime_core_uint16_to_string(self__46)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var t225 string = _goml_runtime_core_float32_to_string(self__49)
    return t225
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var t228 string = _goml_runtime_core_int16_to_string(self__42)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var t231 string = _goml_runtime_core_uint32_to_string(self__47)
    return t231
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var t234 bool = self__69 == other__70
    return t234
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t237 string = _goml_runtime_core_bool_to_string(self__37)
    return t237
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
