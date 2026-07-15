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
    var retv23 int32
    var t24 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv23 = t24
    return retv23
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t26 int32 = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t26)
    println__T_string(t27)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__108 *_goml_vec_int32) int32 {
    var retv30 int32
    var t31 int32 = vec_len__Vec_5int32(self__108)
    retv30 = t31
    return retv30
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int32 {
    var retv36 int32
    var t37 int32 = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv36 = t37
    return retv36
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv39 string
    var t40 string = _goml_runtime_core_int32_to_string(self__2)
    retv39 = t40
    return retv39
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
