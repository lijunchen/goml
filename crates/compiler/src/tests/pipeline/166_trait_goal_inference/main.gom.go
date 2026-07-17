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
    var retv59 int32
    var t60 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv59 = t60
    return retv59
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t62 int32 = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t63 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t62)
    println__T_string(t63)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv66 int32
    var t67 int32 = vec_len__Vec_5int32(self__131)
    retv66 = t67
    return retv66
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int32 {
    var retv72 int32
    var t73 int32 = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv72 = t73
    return retv72
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__2)
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv78 string
    retv78 = self__34
    return retv78
}

func main() {
    main0()
}
