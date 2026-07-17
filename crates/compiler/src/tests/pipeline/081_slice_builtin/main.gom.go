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
    var t73 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int32(t73)
    var t74 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t74)
    var t75 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t75)
    var t76 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t76)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t77 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int32(t77)
    var t78 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t78)
    var t79 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t79)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv81 *_goml_vec_int32
    var t82 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__172 *_goml_vec_int32, start__173 int32, end__174 int32) []int32 {
    var retv86 []int32
    var t87 []int32 = self__172.items[start__173:end__174]
    retv86 = t87
    return retv86
}

func println__T_int32(value__1 int32) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__183 []int32) int32 {
    var retv92 int32
    var t93 int32 = int32(len(self__183))
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__181 []int32, index__182 int32) int32 {
    var retv95 int32
    var t96 int32 = self__181[index__182]
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__184 []int32, start__185 int32, end__186 int32) []int32 {
    var retv98 []int32
    var t99 []int32 = self__184[start__185:end__186]
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__41)
    retv101 = t102
    return retv101
}

func main() {
    main0()
}
