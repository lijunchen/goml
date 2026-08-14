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
    var t200 uint8 = value__0 + 1
    return t200
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp210 uint8
    jp210 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t211 int = default_integer__2 + 2
    var t212 string = _goml_m_inherent_i_int_i_int_i_to__string(t211)
    println__T_string(t212)
    var t213 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t213)
    var t214 uint8 = increment(small__4)
    var t215 string
    var inline323 string = _goml_runtime_core_uint8_to_string(t214)
    t215 = inline323
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline320)
    var t216 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t217 string
    var inline318 string = _goml_runtime_core_uint8_to_string(t216)
    t217 = inline318
    var inline315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline315)
    var t218 string
    var inline313 string = _goml_runtime_core_uint8_to_string(jp210)
    t218 = inline313
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline310)
    var t219 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t220 string
    var inline308 string = _goml_runtime_core_uint16_to_string(t219)
    t220 = inline308
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline305)
    var t221 float32
    t221 = 1
    var t222 string
    var inline302 string = _goml_runtime_core_float32_to_string(t221)
    t222 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline299)
    var t223 int16
    t223 = 2
    var t224 string
    var inline296 string = _goml_runtime_core_int16_to_string(t223)
    t224 = inline296
    var inline293 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline293)
    var t225 uint32
    var inline291 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t225 = inline291
    var t226 string
    var inline289 string = _goml_runtime_core_uint32_to_string(t225)
    t226 = inline289
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline286)
    var t227 bool = byte__9 == 65
    var t228 string
    var inline284 string = _goml_runtime_core_bool_to_string(t227)
    t228 = inline284
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline281)
    var jp230 string
    switch byte__9 {
    case 65:
        jp230 = "byte"
    default:
        jp230 = "other"
    }
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp230)
    _goml_runtime_core_string_println(inline278)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__274 *ref_uint32_x) uint32 {
    var t233 uint32 = ref_get__Ref_6uint32(self__274)
    return t233
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__273 uint32) *ref_uint32_x {
    var t236 *ref_uint32_x = ref__Ref_6uint32(value__273)
    return t236
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t239 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t239
}

func println__T_string(value__1 string) struct{} {
    var t241 string
    t241 = value__1
    _goml_runtime_core_string_println(t241)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t245 string = _goml_runtime_core_int_to_string(self__32)
    return t245
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t248 string = _goml_runtime_core_float64_to_string(self__77)
    return t248
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
