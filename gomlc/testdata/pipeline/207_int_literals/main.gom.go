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
    var t190 uint8 = value__0 + 1
    return t190
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp200 uint8
    jp200 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t201 int = default_integer__2 + 2
    var t202 string = _goml_m_inherent_i_int_i_int_i_to__string(t201)
    println__T_string(t202)
    var t203 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t203)
    var t204 uint8 = increment(small__4)
    var t205 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t204)
    var inline316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline316)
    var t206 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t207 string
    var inline314 string = _goml_runtime_core_uint8_to_string(t206)
    t207 = inline314
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline311)
    var t208 string
    var inline309 string = _goml_runtime_core_uint8_to_string(jp200)
    t208 = inline309
    var inline306 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline306)
    var t209 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t210 string
    var inline304 string = _goml_runtime_core_uint16_to_string(t209)
    t210 = inline304
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline301)
    var t211 float32
    t211 = 1
    var t212 string
    var inline298 string = _goml_runtime_core_float32_to_string(t211)
    t212 = inline298
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline295)
    var t213 int16
    t213 = 2
    var t214 string
    var inline292 string = _goml_runtime_core_int16_to_string(t213)
    t214 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline289)
    var t215 uint32
    var inline287 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t215 = inline287
    var t216 string
    var inline285 string = _goml_runtime_core_uint32_to_string(t215)
    t216 = inline285
    var inline282 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline282)
    var t217 bool
    var inline279 uint8 = 65
    var inline280 bool = byte__9 == inline279
    t217 = inline280
    var t218 string
    var inline277 string = _goml_runtime_core_bool_to_string(t217)
    t218 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline274)
    var jp220 string
    switch byte__9 {
    case 65:
        jp220 = "byte"
    default:
        jp220 = "other"
    }
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp220)
    _goml_runtime_core_string_println(inline271)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__237 *ref_uint32_x) uint32 {
    var t223 uint32 = ref_get__Ref_6uint32(self__237)
    return t223
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__236 uint32) *ref_uint32_x {
    var t226 *ref_uint32_x = ref__Ref_6uint32(value__236)
    return t226
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t229 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t229
}

func println__T_string(value__31 string) struct{} {
    var t231 string
    t231 = value__31
    _goml_runtime_core_string_println(t231)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t235 string = _goml_runtime_core_int_to_string(self__34)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t238 string = _goml_runtime_core_float64_to_string(self__79)
    return t238
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t241 string = _goml_runtime_core_uint8_to_string(self__74)
    return t241
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
