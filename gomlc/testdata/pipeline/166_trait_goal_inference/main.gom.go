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
    var inline204 int = vec_len__Vec_5int32(self__0)
    return inline204
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t186 int
    var inline211 int = _goml_m_trait__impl_i_Measure_i_Vec_l_int32_r__i_measure(values__2)
    t186 = inline211
    var t187 string
    var inline209 string = _goml_runtime_core_int_to_string(t186)
    t187 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
