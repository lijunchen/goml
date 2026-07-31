package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: _goml_slices.Grow([]int32{}, int(capacity)),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func main0() struct{} {
    var to_text__0 func(int32) string = _goml_runtime_core_int32_to_string
    var t164 string = to_text__0(7)
    println__T_string(t164)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t165 int32 = get__1(values__3, 1)
    println__T_int32(t165)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t166 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t166)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t167 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int(t167)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t168 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t168)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t169 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int(t169)
    var t170 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int(t170)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv175 *_goml_vec_int32
    var t176 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv175 = t176
    return retv175
}

func println__T_int32(value__1 int32) struct{} {
    var t178 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t178)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t183 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv186 int
    var t187 int = vec_len__Vec_5int32(self__137)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__134 *_goml_vec_int32, index__135 int, elem__136 int32) struct{} {
    vec_set__Vec_5int32(self__134, index__135, elem__136)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv191 int32
    var t192 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var retv194 *_goml_vec_int32
    var t195 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
    var t196 int = t195 + 1
    var result__130 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(t196)
    var index__131 int = 0
    Loop_loop198:
    for {
        var t199 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
        var t200 bool = index__131 < t199
        if t200 {
            var t201 int32 = vec_get__Vec_5int32(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, t201)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t202 int = compound_old38 + compound_value39
            index__131 = t202
            continue
        } else {
            break Loop_loop198
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, elem__129)
    retv194 = result__130
    return retv194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv205 string
    retv205 = self__38
    return retv205
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv207 string
    var t208 string = _goml_runtime_core_int32_to_string(self__43)
    retv207 = t208
    return retv207
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv210 string
    var t211 string = _goml_runtime_core_int_to_string(self__40)
    retv210 = t211
    return retv210
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var retv213 *_goml_vec_int32
    var t214 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    retv213 = t214
    return retv213
}

func main() {
    main0()
}
