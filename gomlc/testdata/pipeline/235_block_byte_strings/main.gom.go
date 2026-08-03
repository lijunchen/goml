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
    var t223 int = base__0 + 2
    return t223
}

func loop_answer() int {
    var jp227 int
    var base__1 int = 6
    var t229 int = base__1 * 7
    jp227 = t229
    return jp227
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
    var t231 int = loop_answer()
    var t232 int = value__6 + t231
    var t233 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__251)
    var t234 string = _goml_m_inherent_i_int_i_int_i_to__string(t233)
    println__T_string(t234)
    var t235 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__279)
    var t236 string
    var inline323 string = _goml_runtime_core_int_to_string(t235)
    t236 = inline323
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline320)
    var t237 uint8
    var inline317 int = 0
    var inline318 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline317)
    t237 = inline318
    var t238 int = int(uint8(t237))
    var t239 string
    var inline315 string = _goml_runtime_core_int_to_string(t238)
    t239 = inline315
    var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline312)
    var t240 uint8
    var inline309 int = 1
    var inline310 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline309)
    t240 = inline310
    var t241 int = int(uint8(t240))
    var t242 string
    var inline307 string = _goml_runtime_core_int_to_string(t241)
    t242 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline304)
    var t243 uint8
    var inline301 int = 2
    var inline302 uint8 = vec_get__Vec_5uint8(vec_literal__251, inline301)
    t243 = inline302
    var t244 int = int(uint8(t243))
    var t245 string
    var inline299 string = _goml_runtime_core_int_to_string(t244)
    t245 = inline299
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
    _goml_runtime_core_string_println(inline296)
    var t246 int
    var inline294 int = vec_len__Vec_5uint8(vec_literal__298)
    t246 = inline294
    var t247 string
    var inline292 string = _goml_runtime_core_int_to_string(t246)
    t247 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline289)
    var t248 int
    var inline287 int = vec_len__Vec_5uint8(vec_literal__335)
    t248 = inline287
    var t249 string
    var inline285 string = _goml_runtime_core_int_to_string(t248)
    t249 = inline285
    var inline282 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline282)
    var t250 string
    var inline280 string = _goml_runtime_core_int_to_string(t232)
    t250 = inline280
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t250)
    _goml_runtime_core_string_println(inline277)
    var inline273 string = "block condition"
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline273)
    _goml_runtime_core_string_println(inline274)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t255 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t255
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__155 *_goml_vec_uint8, elem__156 uint8) struct{} {
    vec_push__Vec_5uint8(self__155, elem__156)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t259 string
    t259 = value__31
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__166 *_goml_vec_uint8) int {
    var t263 int = vec_len__Vec_5uint8(self__166)
    return t263
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t266 string = _goml_runtime_core_int_to_string(self__34)
    return t266
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
