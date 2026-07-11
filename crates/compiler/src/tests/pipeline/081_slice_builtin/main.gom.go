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

func main0() struct{} {
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 30)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 40)
    var s__1 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(v__0, 1, 4)
    var t16 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int32(t16)
    var t17 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t17)
    var t18 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t18)
    var t19 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t19)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t20 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int32(t20)
    var t21 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t21)
    var t22 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t22)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv24 *_goml_vec_int32
    var t25 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv24 = t25
    return retv24
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__71 *_goml_vec_int32, elem__72 int32) struct{} {
    vec_push__Vec_5int32(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__83 *_goml_vec_int32, start__84 int32, end__85 int32) []int32 {
    var retv29 []int32
    var t30 []int32 = self__83.items[start__84:end__85]
    retv29 = t30
    return retv29
}

func println__T_int32(value__1 int32) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__88 []int32) int32 {
    var retv35 int32
    var t36 int32 = int32(len(self__88))
    retv35 = t36
    return retv35
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__86 []int32, index__87 int32) int32 {
    var retv38 int32
    var t39 int32 = self__86[index__87]
    retv38 = t39
    return retv38
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__89 []int32, start__90 int32, end__91 int32) []int32 {
    var retv41 []int32
    var t42 []int32 = self__89[start__90:end__91]
    retv41 = t42
    return retv41
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv44 string
    var t45 string = _goml_runtime_core_int32_to_string(self__13)
    retv44 = t45
    return retv44
}

func main() {
    main0()
}
