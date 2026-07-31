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
    var retv164 uint8
    var t165 uint8 = value__0 + 1
    retv164 = t165
    return retv164
}

func one_float32() float32 {
    var retv167 float32
    retv167 = 1
    return retv167
}

func two_int16() int16 {
    var retv169 int16
    retv169 = 2
    return retv169
}

func read_uint32(value__1 *ref_uint32_x) uint32 {
    var retv171 uint32
    var t172 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(value__1)
    retv171 = t172
    return retv171
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp175 uint8
    if true {
        jp175 = 1
    } else {
        jp175 = small__4
    }
    var branch__6 uint8 = jp175
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t176 int = default_integer__2 + 2
    var t177 string = _goml_m_inherent_i_int_i_int_i_to__string(t176)
    println__T_string(t177)
    var t178 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t178)
    var t179 uint8 = increment(small__4)
    var t180 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t179)
    println__T_string(t180)
    var t181 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t182 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t181)
    println__T_string(t182)
    var t183 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(branch__6)
    println__T_string(t183)
    var t184 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t185 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(t184)
    println__T_string(t185)
    var t186 float32 = one_float32()
    var t187 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t186)
    println__T_string(t187)
    var t188 int16 = two_int16()
    var t189 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t188)
    println__T_string(t189)
    var t190 uint32 = read_uint32(delayed__8)
    var t191 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t190)
    println__T_string(t191)
    var t192 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__9, 65)
    var t193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    println__T_string(t193)
    var jp195 string
    switch byte__9 {
    case 65:
        jp195 = "byte"
    default:
        jp195 = "other"
    }
    println__T_string(jp195)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__208 *ref_uint32_x) uint32 {
    var retv197 uint32
    var t198 uint32 = ref_get__Ref_6uint32(self__208)
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__207 uint32) *ref_uint32_x {
    var retv200 *ref_uint32_x
    var t201 *ref_uint32_x = ref__Ref_6uint32(value__207)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv203 uint8
    var t204 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv203 = t204
    return retv203
}

func println__T_string(value__1 string) struct{} {
    var t206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv209 string
    var t210 string = _goml_runtime_core_int_to_string(self__5)
    retv209 = t210
    return retv209
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv212 string
    var t213 string = _goml_runtime_core_float64_to_string(self__50)
    retv212 = t213
    return retv212
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv215 string
    var t216 string = _goml_runtime_core_uint8_to_string(self__45)
    retv215 = t216
    return retv215
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv218 string
    var t219 string = _goml_runtime_core_uint16_to_string(self__46)
    retv218 = t219
    return retv218
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv221 string
    var t222 string = _goml_runtime_core_float32_to_string(self__49)
    retv221 = t222
    return retv221
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv224 string
    var t225 string = _goml_runtime_core_int16_to_string(self__42)
    retv224 = t225
    return retv224
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv227 string
    var t228 string = _goml_runtime_core_uint32_to_string(self__47)
    retv227 = t228
    return retv227
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv230 bool
    var t231 bool = self__69 == other__70
    retv230 = t231
    return retv230
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv233 string
    var t234 string = _goml_runtime_core_bool_to_string(self__37)
    retv233 = t234
    return retv233
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv236 string
    retv236 = self__38
    return retv236
}

func main() {
    main0()
}
