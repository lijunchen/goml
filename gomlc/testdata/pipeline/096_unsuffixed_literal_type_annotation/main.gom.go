package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
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

func _goml_runtime_core_uint64_to_string(x uint64) string {
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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func take_u8(x__0 uint8) uint8 {
    var retv163 uint8
    retv163 = x__0
    return retv163
}

func take_f32(x__1 float32) float32 {
    var retv165 float32
    retv165 = x__1
    return retv165
}

func main0() struct{} {
    var a__2 uint8 = 1
    var b__3 int8 = 2
    var c__4 int16 = 3
    var d__5 uint16 = 4
    var e__6 uint32 = 5
    var f__7 int64 = 6
    var g__8 uint64 = 7
    var h__9 float32 = 1
    var t167 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(a__2)
    println__T_string(t167)
    var t168 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__3)
    println__T_string(t168)
    var t169 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(c__4)
    println__T_string(t169)
    var t170 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(d__5)
    println__T_string(t170)
    var t171 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(e__6)
    println__T_string(t171)
    var t172 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(f__7)
    println__T_string(t172)
    var t173 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(g__8)
    println__T_string(t173)
    var t174 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(h__9)
    println__T_string(t174)
    var t175 uint8 = take_u8(10)
    var t176 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t175)
    println__T_string(t176)
    var t177 float32 = take_f32(2.5)
    var t178 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t177)
    println__T_string(t178)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t180)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv183 string
    var t184 string = _goml_runtime_core_uint8_to_string(self__45)
    retv183 = t184
    return retv183
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv186 string
    var t187 string = _goml_runtime_core_int8_to_string(self__41)
    retv186 = t187
    return retv186
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv189 string
    var t190 string = _goml_runtime_core_int16_to_string(self__42)
    retv189 = t190
    return retv189
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv192 string
    var t193 string = _goml_runtime_core_uint16_to_string(self__46)
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_uint32_to_string(self__47)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv198 string
    var t199 string = _goml_runtime_core_int64_to_string(self__44)
    retv198 = t199
    return retv198
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv201 string
    var t202 string = _goml_runtime_core_uint64_to_string(self__48)
    retv201 = t202
    return retv201
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv204 string
    var t205 string = _goml_runtime_core_float32_to_string(self__49)
    retv204 = t205
    return retv204
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv207 string
    retv207 = self__38
    return retv207
}

func main() {
    main0()
}
