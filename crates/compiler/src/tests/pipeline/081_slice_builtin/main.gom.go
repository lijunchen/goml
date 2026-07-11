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
    var t19 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int32(t19)
    var t20 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t20)
    var t21 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t21)
    var t22 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t22)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t23 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int32(t23)
    var t24 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t24)
    var t25 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t25)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv27 *_goml_vec_int32
    var t28 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv27 = t28
    return retv27
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__73 *_goml_vec_int32, elem__74 int32) struct{} {
    vec_push__Vec_5int32(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__85 *_goml_vec_int32, start__86 int32, end__87 int32) []int32 {
    var retv32 []int32
    var t33 []int32 = self__85.items[start__86:end__87]
    retv32 = t33
    return retv32
}

func println__T_int32(value__1 int32) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__95 []int32) int32 {
    var retv38 int32
    var t39 int32 = int32(len(self__95))
    retv38 = t39
    return retv38
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__93 []int32, index__94 int32) int32 {
    var retv41 int32
    var t42 int32 = self__93[index__94]
    retv41 = t42
    return retv41
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__96 []int32, start__97 int32, end__98 int32) []int32 {
    var retv44 []int32
    var t45 []int32 = self__96[start__97:end__98]
    retv44 = t45
    return retv44
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv47 string
    var t48 string = _goml_runtime_core_int32_to_string(self__13)
    retv47 = t48
    return retv47
}

func main() {
    main0()
}
