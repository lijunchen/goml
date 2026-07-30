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
    var retv120 uint8
    var t121 uint8 = value__0 + 1
    retv120 = t121
    return retv120
}

func one_float32() float32 {
    var retv123 float32
    retv123 = 1
    return retv123
}

func two_int16() int16 {
    var retv125 int16
    retv125 = 2
    return retv125
}

func read_uint32(value__1 *ref_uint32_x) uint32 {
    var retv127 uint32
    var t128 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(value__1)
    retv127 = t128
    return retv127
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp131 uint8
    if true {
        jp131 = 1
    } else {
        jp131 = small__4
    }
    var branch__6 uint8 = jp131
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t132 int = default_integer__2 + 2
    var t133 string = _goml_m_inherent_i_int_i_int_i_to__string(t132)
    println__T_string(t133)
    var t134 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t134)
    var t135 uint8 = increment(small__4)
    var t136 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t135)
    println__T_string(t136)
    var t137 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t138 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t137)
    println__T_string(t138)
    var t139 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(branch__6)
    println__T_string(t139)
    var t140 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t141 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(t140)
    println__T_string(t141)
    var t142 float32 = one_float32()
    var t143 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t142)
    println__T_string(t143)
    var t144 int16 = two_int16()
    var t145 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t144)
    println__T_string(t145)
    var t146 uint32 = read_uint32(delayed__8)
    var t147 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t146)
    println__T_string(t147)
    var t148 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__9, 65)
    var t149 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t148)
    println__T_string(t149)
    var jp151 string
    switch byte__9 {
    case 65:
        jp151 = "byte"
    default:
        jp151 = "other"
    }
    println__T_string(jp151)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__208 *ref_uint32_x) uint32 {
    var retv153 uint32
    var t154 uint32 = ref_get__Ref_6uint32(self__208)
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__207 uint32) *ref_uint32_x {
    var retv156 *ref_uint32_x
    var t157 *ref_uint32_x = ref__Ref_6uint32(value__207)
    retv156 = t157
    return retv156
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv159 uint8
    var t160 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv159 = t160
    return retv159
}

func println__T_string(value__1 string) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv165 string
    var t166 string = _goml_runtime_core_int_to_string(self__5)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv168 string
    var t169 string = _goml_runtime_core_float64_to_string(self__50)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv171 string
    var t172 string = _goml_runtime_core_uint8_to_string(self__45)
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv174 string
    var t175 string = _goml_runtime_core_uint16_to_string(self__46)
    retv174 = t175
    return retv174
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv177 string
    var t178 string = _goml_runtime_core_float32_to_string(self__49)
    retv177 = t178
    return retv177
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv180 string
    var t181 string = _goml_runtime_core_int16_to_string(self__42)
    retv180 = t181
    return retv180
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv183 string
    var t184 string = _goml_runtime_core_uint32_to_string(self__47)
    retv183 = t184
    return retv183
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv186 bool
    var t187 bool = self__69 == other__70
    retv186 = t187
    return retv186
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv189 string
    var t190 string = _goml_runtime_core_bool_to_string(self__37)
    retv189 = t190
    return retv189
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv192 string
    retv192 = self__38
    return retv192
}

func main() {
    main0()
}
