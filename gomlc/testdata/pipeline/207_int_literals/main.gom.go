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
    var t195 uint8 = value__0 + 1
    return t195
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp205 uint8
    jp205 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t206 int = default_integer__2 + 2
    var t207 string = _goml_m_inherent_i_int_i_int_i_to__string(t206)
    println__T_string(t207)
    var t208 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t208)
    var t209 uint8 = increment(small__4)
    var t210 string
    var inline318 string = _goml_runtime_core_uint8_to_string(t209)
    t210 = inline318
    var inline315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline315)
    var t211 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t212 string
    var inline313 string = _goml_runtime_core_uint8_to_string(t211)
    t212 = inline313
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline310)
    var t213 string
    var inline308 string = _goml_runtime_core_uint8_to_string(jp205)
    t213 = inline308
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline305)
    var t214 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t215 string
    var inline303 string = _goml_runtime_core_uint16_to_string(t214)
    t215 = inline303
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline300)
    var t216 float32
    t216 = 1
    var t217 string
    var inline297 string = _goml_runtime_core_float32_to_string(t216)
    t217 = inline297
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline294)
    var t218 int16
    t218 = 2
    var t219 string
    var inline291 string = _goml_runtime_core_int16_to_string(t218)
    t219 = inline291
    var inline288 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline288)
    var t220 uint32
    var inline286 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t220 = inline286
    var t221 string
    var inline284 string = _goml_runtime_core_uint32_to_string(t220)
    t221 = inline284
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline281)
    var t222 bool = byte__9 == 65
    var t223 string
    var inline279 string = _goml_runtime_core_bool_to_string(t222)
    t223 = inline279
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline276)
    var jp225 string
    switch byte__9 {
    case 65:
        jp225 = "byte"
    default:
        jp225 = "other"
    }
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp225)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__271 *ref_uint32_x) uint32 {
    var t228 uint32 = ref_get__Ref_6uint32(self__271)
    return t228
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__270 uint32) *ref_uint32_x {
    var t231 *ref_uint32_x = ref__Ref_6uint32(value__270)
    return t231
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t234 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t234
}

func println__T_string(value__1 string) struct{} {
    var t236 string
    t236 = value__1
    _goml_runtime_core_string_println(t236)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t240 string = _goml_runtime_core_int_to_string(self__32)
    return t240
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t243 string = _goml_runtime_core_float64_to_string(self__77)
    return t243
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
