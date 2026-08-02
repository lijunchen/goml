package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(self__0 *_goml_vec_int32) int {
    var t157 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    return t157
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t159 int = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t160 string = _goml_m_inherent_i_int_i_int_i_to__string(t159)
    println__T_string(t160)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var t164 int = vec_len__Vec_5int32(self__137)
    return t164
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int {
    var t170 int = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    return t170
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t173 string = _goml_runtime_core_int_to_string(self__5)
    return t173
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
