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
    var t34 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int32(t34)
    var t35 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t35)
    var t36 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t36)
    var t37 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t37)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t38 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int32(t38)
    var t39 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t39)
    var t40 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t40)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv42 *_goml_vec_int32
    var t43 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv42 = t43
    return retv42
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__97 *_goml_vec_int32, elem__98 int32) struct{} {
    vec_push__Vec_5int32(self__97, elem__98)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__109 *_goml_vec_int32, start__110 int32, end__111 int32) []int32 {
    var retv47 []int32
    var t48 []int32 = self__109.items[start__110:end__111]
    retv47 = t48
    return retv47
}

func println__T_int32(value__1 int32) struct{} {
    var t50 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t50)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__120 []int32) int32 {
    var retv53 int32
    var t54 int32 = int32(len(self__120))
    retv53 = t54
    return retv53
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__118 []int32, index__119 int32) int32 {
    var retv56 int32
    var t57 int32 = self__118[index__119]
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__121 []int32, start__122 int32, end__123 int32) []int32 {
    var retv59 []int32
    var t60 []int32 = self__121[start__122:end__123]
    retv59 = t60
    return retv59
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv62 string
    var t63 string = _goml_runtime_core_int32_to_string(self__13)
    retv62 = t63
    return retv62
}

func main() {
    main0()
}
