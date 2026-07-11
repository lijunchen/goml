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

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

func _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(self__0 *_goml_vec_int32) int32 {
    var retv5 int32
    var t6 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv5 = t6
    return retv5
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t8 int32 = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t9 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t8)
    println__T_string(t9)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__82 *_goml_vec_int32) int32 {
    var retv12 int32
    var t13 int32 = vec_len__Vec_5int32(self__82)
    retv12 = t13
    return retv12
}

func println__T_string(value__1 string) struct{} {
    var t15 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t15)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int32 {
    var retv18 int32
    var t19 int32 = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv18 = t19
    return retv18
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv21 string
    var t22 string = _goml_runtime_core_int32_to_string(self__2)
    retv21 = t22
    return retv21
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv24 string
    retv24 = self__9
    return retv24
}

func main() {
    main0()
}
