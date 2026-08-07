package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

func answer() int {
    var base__0 int = 40
    var t182 int = base__0 + 2
    return t182
}

func loop_answer() int {
    var jp186 int
    var base__1 int = 6
    var t188 int = base__1 * 7
    jp186 = t188
    return jp186
}

func main0() struct{} {
    var vec_literal__251 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__251, 65)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__251, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__251, 66)
    var vec_literal__279 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var vec_literal__298 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 114)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 97)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 119)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 32)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 92)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 110)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 32)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 98)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 121)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 116)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 101)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__298, 115)
    var vec_literal__335 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 113)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 117)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 111)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 116)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 101)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 100)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 32)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 34)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 116)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 101)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 120)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 116)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 34)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 32)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 97)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 110)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 100)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 32)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__335, 35)
    var value__6 int = answer()
    var t190 int = loop_answer()
    var t191 int = value__6 + t190
    var t192 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__251)
    var t193 string = _goml_m_inherent_i_int_i_int_i_to__string(t192)
    println__T_string(t193)
    var t194 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__279)
    var t195 string
    var inline282 string = _goml_runtime_core_int_to_string(t194)
    t195 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline279)
    var t196 uint8
    var inline276 int = 0
    var inline277 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline276)
    t196 = inline277
    var t197 int = int(uint8(t196))
    var t198 string
    var inline274 string = _goml_runtime_core_int_to_string(t197)
    t198 = inline274
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline271)
    var t199 uint8
    var inline268 int = 1
    var inline269 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline268)
    t199 = inline269
    var t200 int = int(uint8(t199))
    var t201 string
    var inline266 string = _goml_runtime_core_int_to_string(t200)
    t201 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline263)
    var t202 uint8
    var inline260 int = 2
    var inline261 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline260)
    t202 = inline261
    var t203 int = int(uint8(t202))
    var t204 string
    var inline258 string = _goml_runtime_core_int_to_string(t203)
    t204 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline255)
    var t205 int
    var inline253 int = vec_len__Vec_5uint8(vec_literal__298)
    t205 = inline253
    var t206 string
    var inline251 string = _goml_runtime_core_int_to_string(t205)
    t206 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline248)
    var t207 int
    var inline246 int = vec_len__Vec_5uint8(vec_literal__335)
    t207 = inline246
    var t208 string
    var inline244 string = _goml_runtime_core_int_to_string(t207)
    t208 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline241)
    var t209 string
    var inline239 string = _goml_runtime_core_int_to_string(t191)
    t209 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline236)
    var inline232 string = "block condition"
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline232)
    _goml_runtime_core_string_println(inline233)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t214 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t214
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__151 *_goml_vec_uint8, elem__152 uint8) struct{} {
    vec_push__Vec_5uint8(self__151, elem__152)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t218 string
    t218 = value__31
    _goml_runtime_core_string_println(t218)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__162 *_goml_vec_uint8) int {
    var t222 int = vec_len__Vec_5uint8(self__162)
    return t222
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t225 string = _goml_runtime_core_int_to_string(self__34)
    return t225
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
