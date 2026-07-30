package main

import (
    _goml_fmt "fmt"
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
    var t120 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int(t120)
    var t121 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t121)
    var t122 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t122)
    var t123 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t123)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t124 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int(t124)
    var t125 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t125)
    var t126 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t126)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv128 *_goml_vec_int32
    var t129 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv128 = t129
    return retv128
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv133 []int32
    var t134 []int32 = self__175.items[start__176:end__177]
    retv133 = t134
    return retv133
}

func println__T_int(value__1 int) struct{} {
    var t136 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t136)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv139 int
    var t140 int = len(self__186)
    retv139 = t140
    return retv139
}

func println__T_int32(value__1 int32) struct{} {
    var t142 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t142)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__184 []int32, index__185 int) int32 {
    var retv145 int32
    var t146 int32 = self__184[index__185]
    retv145 = t146
    return retv145
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__187 []int32, start__188 int, end__189 int) []int32 {
    var retv148 []int32
    var t149 []int32 = self__187[start__188:end__189]
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int_to_string(self__40)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv154 string
    var t155 string = _goml_runtime_core_int32_to_string(self__43)
    retv154 = t155
    return retv154
}

func main() {
    main0()
}
