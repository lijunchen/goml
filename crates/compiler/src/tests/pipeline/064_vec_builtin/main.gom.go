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
    var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(first__1)
    println__T_string(t66)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(second__2)
    println__T_string(t67)
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(third__3)
    println__T_string(t68)
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len__4)
    println__T_string(t69)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv71 *_goml_vec_int32
    var t72 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv71 = t72
    return retv71
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv76 int32
    var t77 int32 = vec_len__Vec_5int32(self__131)
    retv76 = t77
    return retv76
}

func println__T_string(value__1 string) struct{} {
    var t79 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int32_to_string(self__2)
    retv82 = t83
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv85 string
    retv85 = self__34
    return retv85
}

func main() {
    main0()
}
