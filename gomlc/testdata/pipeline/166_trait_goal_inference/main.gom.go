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
    var retv153 int
    var t154 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    retv153 = t154
    return retv153
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t156 int = _goml_m_read__measure____T__Vec_l_int32_r_(values__2)
    var t157 string = _goml_m_inherent_i_int_i_int_i_to__string(t156)
    println__T_string(t157)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv160 int
    var t161 int = vec_len__Vec_5int32(self__137)
    retv160 = t161
    return retv160
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func _goml_m_read__measure____T__Vec_l_int32_r_(value__1 *_goml_vec_int32) int {
    var retv166 int
    var t167 int = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(value__1)
    retv166 = t167
    return retv166
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int_to_string(self__5)
    retv169 = t170
    return retv169
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv172 string
    retv172 = self__38
    return retv172
}

func main() {
    main0()
}
