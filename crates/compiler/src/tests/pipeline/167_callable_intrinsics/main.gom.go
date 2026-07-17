package main

import (
    _goml_fmt "fmt"
)

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

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int32, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var to_text__0 func(int32) string = _goml_runtime_core_int32_to_string
    var t70 string = to_text__0(7)
    println__T_string(t70)
    var get__1 func(*_goml_vec_int32, int32) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t71 int32 = get__1(values__3, 1)
    println__T_int32(t71)
    var make_slice__4 func(*_goml_vec_int32, int32, int32) []int32 = func(p0 *_goml_vec_int32, p1 int32, p2 int32) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int32) int32 = func(p0 []int32, p1 int32) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t72 int32 = slice_get_value__5(view__6, 0)
    println__T_int32(t72)
    var alias__7 *_goml_vec_int32 = values__3
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(alias__7, 33)
    var t73 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t73)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(values__3, 0, 44)
    var t74 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(alias__7, 0)
    println__T_int32(t74)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t75 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__3)
    println__T_int32(t75)
    var t76 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(copied__8)
    println__T_int32(t76)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv81 *_goml_vec_int32
    var t82 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv81 = t82
    return retv81
}

func println__T_int32(value__1 int32) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv89 int32
    var t90 int32 = vec_len__Vec_5int32(self__131)
    retv89 = t90
    return retv89
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__128 *_goml_vec_int32, index__129 int32, elem__130 int32) struct{} {
    vec_set__Vec_5int32(self__128, index__129, elem__130)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv94 int32
    var t95 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv94 = t95
    return retv94
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__122 *_goml_vec_int32, elem__123 int32) *_goml_vec_int32 {
    var retv97 *_goml_vec_int32
    var result__124 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop99:
    for {
        var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t101 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__122)
        var t102 bool = t100 < t101
        if t102 {
            var t103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t104 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__122, t103)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__124, t104)
            var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t106 int32 = t105 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t106)
            continue
        } else {
            break Loop_loop99
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__124, elem__123)
    retv97 = result__124
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv108 string
    retv108 = self__34
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int32_to_string(self__38)
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv113 *ref_int32_x
    var t114 *ref_int32_x = ref__Ref_5int32(value__201)
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv116 int32
    var t117 int32 = ref_get__Ref_5int32(self__202)
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func main() {
    main0()
}
