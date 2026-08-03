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
    var inline158 int = vec_len__Vec_5int32(self__0)
    return inline158
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t140 int
    var inline165 int = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(values__2)
    t140 = inline165
    var t141 string
    var inline163 string = _goml_runtime_core_int_to_string(t140)
    t141 = inline163
    var inline160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t141)
    _goml_runtime_core_string_println(inline160)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
