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
    var t185 uint8 = value__0 + 1
    return t185
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp195 uint8
    jp195 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t196 int = default_integer__2 + 2
    var t197 string = _goml_m_inherent_i_int_i_int_i_to__string(t196)
    println__T_string(t197)
    var t198 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t198)
    var t199 uint8 = increment(small__4)
    var t200 string
    var inline308 string = _goml_runtime_core_uint8_to_string(t199)
    t200 = inline308
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline305)
    var t201 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t202 string
    var inline303 string = _goml_runtime_core_uint8_to_string(t201)
    t202 = inline303
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline300)
    var t203 string
    var inline298 string = _goml_runtime_core_uint8_to_string(jp195)
    t203 = inline298
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline295)
    var t204 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t205 string
    var inline293 string = _goml_runtime_core_uint16_to_string(t204)
    t205 = inline293
    var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline290)
    var t206 float32
    t206 = 1
    var t207 string
    var inline287 string = _goml_runtime_core_float32_to_string(t206)
    t207 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline284)
    var t208 int16
    t208 = 2
    var t209 string
    var inline281 string = _goml_runtime_core_int16_to_string(t208)
    t209 = inline281
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline278)
    var t210 uint32
    var inline276 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t210 = inline276
    var t211 string
    var inline274 string = _goml_runtime_core_uint32_to_string(t210)
    t211 = inline274
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline271)
    var t212 bool = byte__9 == 65
    var t213 string
    var inline269 string = _goml_runtime_core_bool_to_string(t212)
    t213 = inline269
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline266)
    var jp215 string
    switch byte__9 {
    case 65:
        jp215 = "byte"
    default:
        jp215 = "other"
    }
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp215)
    _goml_runtime_core_string_println(inline263)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__258 *ref_uint32_x) uint32 {
    var t218 uint32 = ref_get__Ref_6uint32(self__258)
    return t218
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__257 uint32) *ref_uint32_x {
    var t221 *ref_uint32_x = ref__Ref_6uint32(value__257)
    return t221
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t224 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t224
}

func println__T_string(value__31 string) struct{} {
    var t226 string
    t226 = value__31
    _goml_runtime_core_string_println(t226)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t230 string = _goml_runtime_core_int_to_string(self__34)
    return t230
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t233 string = _goml_runtime_core_float64_to_string(self__79)
    return t233
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
