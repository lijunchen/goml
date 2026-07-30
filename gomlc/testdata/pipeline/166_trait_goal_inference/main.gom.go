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
    var retv109 int
    var t110 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv109 = t110
    return retv109
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t112 int = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t113 string = _goml_m_inherent_i_int_i_int_i_to__string(t112)
    println__T_string(t113)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv116 int
    var t117 int = vec_len__Vec_5int32(self__137)
    retv116 = t117
    return retv116
}

func println__T_string(value__1 string) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int {
    var retv122 int
    var t123 int = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv122 = t123
    return retv122
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int_to_string(self__5)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func main() {
    main0()
}
