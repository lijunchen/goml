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
    var t149 uint8 = value__0 + 1
    return t149
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp159 uint8
    jp159 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t160 int = default_integer__2 + 2
    var t161 string = _goml_m_inherent_i_int_i_int_i_to__string(t160)
    println__T_string(t161)
    var t162 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t162)
    var t163 uint8 = increment(small__4)
    var t164 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t163)
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline275)
    var t165 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t166 string
    var inline273 string = _goml_runtime_core_uint8_to_string(t165)
    t166 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline270)
    var t167 string
    var inline268 string = _goml_runtime_core_uint8_to_string(jp159)
    t167 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline265)
    var t168 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t169 string
    var inline263 string = _goml_runtime_core_uint16_to_string(t168)
    t169 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline260)
    var t170 float32
    t170 = 1
    var t171 string
    var inline257 string = _goml_runtime_core_float32_to_string(t170)
    t171 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline254)
    var t172 int16
    t172 = 2
    var t173 string
    var inline251 string = _goml_runtime_core_int16_to_string(t172)
    t173 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline248)
    var t174 uint32
    var inline246 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(delayed__8)
    t174 = inline246
    var t175 string
    var inline244 string = _goml_runtime_core_uint32_to_string(t174)
    t175 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline241)
    var t176 bool
    var inline238 uint8 = 65
    var inline239 bool = byte__9 == inline238
    t176 = inline239
    var t177 string
    var inline236 string = _goml_runtime_core_bool_to_string(t176)
    t177 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline233)
    var jp179 string
    switch byte__9 {
    case 65:
        jp179 = "byte"
    default:
        jp179 = "other"
    }
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp179)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__216 *ref_uint32_x) uint32 {
    var t182 uint32 = ref_get__Ref_6uint32(self__216)
    return t182
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__215 uint32) *ref_uint32_x {
    var t185 *ref_uint32_x = ref__Ref_6uint32(value__215)
    return t185
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t188 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t188
}

func println__T_string(value__31 string) struct{} {
    var t190 string
    t190 = value__31
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t194 string = _goml_runtime_core_int_to_string(self__34)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t197 string = _goml_runtime_core_float64_to_string(self__79)
    return t197
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t200 string = _goml_runtime_core_uint8_to_string(self__74)
    return t200
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
