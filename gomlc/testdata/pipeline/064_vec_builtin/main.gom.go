package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func main0() struct{} {
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 30)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(v__0)
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first__1)
    println__T_string(t72)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second__2)
    println__T_string(t73)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(third__3)
    println__T_string(t74)
    var t75 string = _goml_m_inherent_i_int_i_int_i_to__string(len__4)
    println__T_string(t75)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv77 *_goml_vec_int32
    var t78 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv82 int
    var t83 int = vec_len__Vec_5int32(self__139)
    retv82 = t83
    return retv82
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv88 string
    var t89 string = _goml_runtime_core_int32_to_string(self__6)
    retv88 = t89
    return retv88
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int_to_string(self__5)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv94 string
    retv94 = self__38
    return retv94
}

func main() {
    main0()
}
