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
    var t164 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(s__1)
    println__T_int(t164)
    var t165 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 0)
    println__T_int32(t165)
    var t166 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 1)
    println__T_int32(t166)
    var t167 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(s__1, 2)
    println__T_int32(t167)
    var t__2 []int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(s__1, 1, 3)
    var t168 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(t__2)
    println__T_int(t168)
    var t169 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 0)
    println__T_int32(t169)
    var t170 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(t__2, 1)
    println__T_int32(t170)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv172 *_goml_vec_int32
    var t173 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv177 []int32
    var t178 []int32 = self__175.items[start__176:end__177]
    retv177 = t178
    return retv177
}

func println__T_int(value__1 int) struct{} {
    var t180 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t180)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv183 int
    var t184 int = len(self__186)
    retv183 = t184
    return retv183
}

func println__T_int32(value__1 int32) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__184 []int32, index__185 int) int32 {
    var retv189 int32
    var t190 int32 = self__184[index__185]
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__int32(self__187 []int32, start__188 int, end__189 int) []int32 {
    var retv192 []int32
    var t193 []int32 = self__187[start__188:end__189]
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int_to_string(self__40)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv198 string
    var t199 string = _goml_runtime_core_int32_to_string(self__43)
    retv198 = t199
    return retv198
}

func main() {
    main0()
}
