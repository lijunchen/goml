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
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline294)
    var t184 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t185 string
    var inline292 string = _goml_runtime_core_uint8_to_string(t184)
    t185 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline289)
    var t186 string
    var inline287 string = _goml_runtime_core_uint8_to_string(jp178)
    t186 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline284)
    var t187 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t188 string
    var inline282 string = _goml_runtime_core_uint16_to_string(t187)
    t188 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline279)
    var t189 float32
    t189 = 1
    var t190 string
    var inline276 string = _goml_runtime_core_float32_to_string(t189)
    t190 = inline276
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline273)
    var t191 int16
    t191 = 2
    var t192 string
    var inline270 string = _goml_runtime_core_int16_to_string(t191)
    t192 = inline270
    var inline267 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline267)
    var t193 uint32
    var inline265 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t193 = inline265
    var t194 string
    var inline263 string = _goml_runtime_core_uint32_to_string(t193)
    t194 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline260)
    var t195 bool
    var inline257 uint8 = 65
    var inline258 bool = byte__9 == inline257
    t195 = inline258
    var t196 string
    var inline255 string = _goml_runtime_core_bool_to_string(t195)
    t196 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline252)
    var jp198 string
    switch byte__9 {
    case 65:
        jp198 = "byte"
    default:
        jp198 = "other"
    }
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp198)
    _goml_runtime_core_string_println(inline249)
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
    var t209 string
    t209 = value__1
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

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
