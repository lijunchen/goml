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
    var t70 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int32(t70)
    var t71 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t71)
    var t72 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t72)
    var t73 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t73)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t74 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int32(t74)
    var t75 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t75)
    var t76 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t76)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv78 *_goml_vec_int32
    var t79 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__169 *_goml_vec_int32, start__170 int32, end__171 int32) []int32 {
    var retv83 []int32
    var t84 []int32 = self__169.items[start__170:end__171]
    retv83 = t84
    return retv83
}

func println__T_int32(value__1 int32) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__180 []int32) int32 {
    var retv89 int32
    var t90 int32 = int32(len(self__180))
    retv89 = t90
    return retv89
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__178 []int32, index__179 int32) int32 {
    var retv92 int32
    var t93 int32 = self__178[index__179]
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__181 []int32, start__182 int32, end__183 int32) []int32 {
    var retv95 []int32
    var t96 []int32 = self__181[start__182:end__183]
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int32_to_string(self__38)
    retv98 = t99
    return retv98
}

func main() {
    main0()
}
