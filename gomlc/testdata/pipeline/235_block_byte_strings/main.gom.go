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
    var t218 int = base__0 + 2
    return t218
}

func loop_answer() int {
    var jp222 int
    var base__1 int = 6
    var t224 int = base__1 * 7
    jp222 = t224
    return jp222
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
    var t226 int = loop_answer()
    var t227 int = value__6 + t226
    var t228 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__251)
    var t229 string = _goml_m_inherent_i_int_i_int_i_to__string(t228)
    println__T_string(t229)
    var t230 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__279)
    var t231 string
    var inline318 string = _goml_runtime_core_int_to_string(t230)
    t231 = inline318
    var inline315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline315)
    var t232 uint8
    var inline312 int = 0
    var inline313 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline312)
    t232 = inline313
    var t233 int = int(uint8(t232))
    var t234 string
    var inline310 string = _goml_runtime_core_int_to_string(t233)
    t234 = inline310
    var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline307)
    var t235 uint8
    var inline304 int = 1
    var inline305 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline304)
    t235 = inline305
    var t236 int = int(uint8(t235))
    var t237 string
    var inline302 string = _goml_runtime_core_int_to_string(t236)
    t237 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline299)
    var t238 uint8
    var inline296 int = 2
    var inline297 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline296)
    t238 = inline297
    var t239 int = int(uint8(t238))
    var t240 string
    var inline294 string = _goml_runtime_core_int_to_string(t239)
    t240 = inline294
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline291)
    var t241 int
    var inline289 int = vec_len__Vec_5uint8(vec_literal__298)
    t241 = inline289
    var t242 string
    var inline287 string = _goml_runtime_core_int_to_string(t241)
    t242 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline284)
    var t243 int
    var inline282 int = vec_len__Vec_5uint8(vec_literal__335)
    t243 = inline282
    var t244 string
    var inline280 string = _goml_runtime_core_int_to_string(t243)
    t244 = inline280
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline277)
    var t245 string
    var inline275 string = _goml_runtime_core_int_to_string(t227)
    t245 = inline275
    var inline272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
    _goml_runtime_core_string_println(inline272)
    var inline268 string = "block condition"
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline268)
    _goml_runtime_core_string_println(inline269)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t250 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t250
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__176 *_goml_vec_uint8, elem__177 uint8) struct{} {
    vec_push__Vec_5uint8(self__176, elem__177)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t254 string
    t254 = value__31
    _goml_runtime_core_string_println(t254)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__187 *_goml_vec_uint8) int {
    var t258 int = vec_len__Vec_5uint8(self__187)
    return t258
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t261 string = _goml_runtime_core_int_to_string(self__34)
    return t261
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
