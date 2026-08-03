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
    var inline199 int = vec_len__Vec_5int32(self__0)
    return inline199
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t181 int
    var inline206 int = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(values__2)
    t181 = inline206
    var t182 string
    var inline204 string = _goml_runtime_core_int_to_string(t181)
    t182 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
