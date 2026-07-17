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

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

func main0() struct{} {
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__0, 30)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(v__0)
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first__1)
    println__T_string(t69)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second__2)
    println__T_string(t70)
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(third__3)
    println__T_string(t71)
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len__4)
    println__T_string(t72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv74 *_goml_vec_int32
    var t75 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv74 = t75
    return retv74
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv79 int32
    var t80 int32 = vec_len__Vec_5int32(self__134)
    retv79 = t80
    return retv79
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv85 string
    var t86 string = _goml_runtime_core_int32_to_string(self__5)
    retv85 = t86
    return retv85
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv88 string
    retv88 = self__37
    return retv88
}

func main() {
    main0()
}
