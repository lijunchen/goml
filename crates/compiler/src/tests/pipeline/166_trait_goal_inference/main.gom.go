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
    var retv62 int32
    var t63 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv62 = t63
    return retv62
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t65 int32 = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t65)
    println__T_string(t66)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv69 int32
    var t70 int32 = vec_len__Vec_5int32(self__134)
    retv69 = t70
    return retv69
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int32 {
    var retv75 int32
    var t76 int32 = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv75 = t76
    return retv75
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__5)
    retv78 = t79
    return retv78
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv81 string
    retv81 = self__37
    return retv81
}

func main() {
    main0()
}
