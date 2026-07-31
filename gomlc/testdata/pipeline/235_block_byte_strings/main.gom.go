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
    var retv197 int
    var base__0 int = 40
    var t198 int = base__0 + 2
    retv197 = t198
    return retv197
}

func loop_answer() int {
    var retv200 int
    var jp202 int
    var base__1 int = 6
    var t204 int = base__1 * 7
    jp202 = t204
    retv200 = jp202
    return retv200
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
    var t206 int = loop_answer()
    var t207 int = value__6 + t206
    var nested__7 int = t207
    var t208 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(plain__2)
    var t209 string = _goml_m_inherent_i_int_i_int_i_to__string(t208)
    println__T_string(t209)
    var t210 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(empty__3)
    var t211 string = _goml_m_inherent_i_int_i_int_i_to__string(t210)
    println__T_string(t211)
    var t212 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(plain__2, 0)
    var t213 int = int(uint8(t212))
    var t214 string = _goml_m_inherent_i_int_i_int_i_to__string(t213)
    println__T_string(t214)
    var t215 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(plain__2, 1)
    var t216 int = int(uint8(t215))
    var t217 string = _goml_m_inherent_i_int_i_int_i_to__string(t216)
    println__T_string(t217)
    var t218 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(plain__2, 2)
    var t219 int = int(uint8(t218))
    var t220 string = _goml_m_inherent_i_int_i_int_i_to__string(t219)
    println__T_string(t220)
    var t221 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(raw__4)
    var t222 string = _goml_m_inherent_i_int_i_int_i_to__string(t221)
    println__T_string(t222)
    var t223 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(quoted__5)
    var t224 string = _goml_m_inherent_i_int_i_int_i_to__string(t223)
    println__T_string(t224)
    var t225 string = _goml_m_inherent_i_int_i_int_i_to__string(nested__7)
    println__T_string(t225)
    if true {
        println__T_string("block condition")
    } else {}
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv229 *_goml_vec_uint8
    var t230 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__137 *_goml_vec_uint8) int {
    var retv237 int
    var t238 int = vec_len__Vec_5uint8(self__137)
    retv237 = t238
    return retv237
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv240 string
    var t241 string = _goml_runtime_core_int_to_string(self__5)
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__132 *_goml_vec_uint8, index__133 int) uint8 {
    var retv243 uint8
    var t244 uint8 = vec_get__Vec_5uint8(self__132, index__133)
    retv243 = t244
    return retv243
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv246 string
    retv246 = self__38
    return retv246
}

func main() {
    main0()
}
