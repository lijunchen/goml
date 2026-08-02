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
    var retv200 int
    var base__0 int = 40
    var t201 int = base__0 + 2
    retv200 = t201
    return retv200
}

func loop_answer() int {
    var retv203 int
    var jp205 int
    var base__1 int = 6
    var t207 int = base__1 * 7
    jp205 = t207
    retv203 = jp205
    return retv203
}

func main0() struct{} {
    var vec_literal__251 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__251, 65)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__251, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__251, 66)
    var plain__2 *_goml_vec_uint8 = vec_literal__251
    var vec_literal__279 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var empty__3 *_goml_vec_uint8 = vec_literal__279
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
    var raw__4 *_goml_vec_uint8 = vec_literal__298
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
    var quoted__5 *_goml_vec_uint8 = vec_literal__335
    var value__6 int = answer()
    var t209 int = loop_answer()
    var t210 int = value__6 + t209
    var nested__7 int = t210
    var t211 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(plain__2)
    var t212 string = _goml_m_inherent_i_int_i_int_i_to__string(t211)
    println__T_string(t212)
    var t213 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(empty__3)
    var t214 string = _goml_m_inherent_i_int_i_int_i_to__string(t213)
    println__T_string(t214)
    var t215 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(plain__2, 0)
    var t216 int = int(uint8(t215))
    var t217 string = _goml_m_inherent_i_int_i_int_i_to__string(t216)
    println__T_string(t217)
    var t218 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(plain__2, 1)
    var t219 int = int(uint8(t218))
    var t220 string = _goml_m_inherent_i_int_i_int_i_to__string(t219)
    println__T_string(t220)
    var t221 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(plain__2, 2)
    var t222 int = int(uint8(t221))
    var t223 string = _goml_m_inherent_i_int_i_int_i_to__string(t222)
    println__T_string(t223)
    var t224 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(raw__4)
    var t225 string = _goml_m_inherent_i_int_i_int_i_to__string(t224)
    println__T_string(t225)
    var t226 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(quoted__5)
    var t227 string = _goml_m_inherent_i_int_i_int_i_to__string(t226)
    println__T_string(t227)
    var t228 string = _goml_m_inherent_i_int_i_int_i_to__string(nested__7)
    println__T_string(t228)
    if true {
        println__T_string("block condition")
    } else {}
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv232 *_goml_vec_uint8
    var t233 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t237)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__137 *_goml_vec_uint8) int {
    var retv240 int
    var t241 int = vec_len__Vec_5uint8(self__137)
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv243 string
    var t244 string = _goml_runtime_core_int_to_string(self__5)
    retv243 = t244
    return retv243
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__132 *_goml_vec_uint8, index__133 int) uint8 {
    var retv246 uint8
    var t247 uint8 = vec_get__Vec_5uint8(self__132, index__133)
    retv246 = t247
    return retv246
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv249 string
    retv249 = self__38
    return retv249
}

func main() {
    main0()
}
