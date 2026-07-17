package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int32) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
}

type _goml_vec_bool struct {
    items []bool
}

func vec_new__Vec_4bool() *_goml_vec_bool {
    return &_goml_vec_bool{
        items: nil,
    }
}

func vec_push__Vec_4bool(vec *_goml_vec_bool, elem bool) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_4bool(vec *_goml_vec_bool, index int32) bool {
    return vec.items[index]
}

func vec_len__Vec_4bool(vec *_goml_vec_bool) int32 {
    return int32(len(vec.items))
}

func main0() struct{} {
    var vi__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(vi__0, 42)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(vi__0)
    var vs__3 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vs__3, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vs__3, "world")
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(vs__3)
    var vb__6 *_goml_vec_bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__bool()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(vb__6, true)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(vb__6, false)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__bool(vb__6)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(val_i__1)
    println__T_string(t70)
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_i__2)
    println__T_string(t71)
    println__T_string(val_s__4)
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_s__5)
    println__T_string(t72)
    var t73 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(val_b__7)
    println__T_string(t73)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_b__8)
    println__T_string(t74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv76 *_goml_vec_int32
    var t77 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv76 = t77
    return retv76
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv81 int32
    var t82 int32 = vec_len__Vec_5int32(self__131)
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv84 *_goml_vec_string
    var t85 *_goml_vec_string = vec_new__Vec_6string()
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__120 *_goml_vec_string, elem__121 string) struct{} {
    vec_push__Vec_6string(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__131 *_goml_vec_string) int32 {
    var retv89 int32
    var t90 int32 = vec_len__Vec_6string(self__131)
    retv89 = t90
    return retv89
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__bool() *_goml_vec_bool {
    var retv92 *_goml_vec_bool
    var t93 *_goml_vec_bool = vec_new__Vec_4bool()
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(self__120 *_goml_vec_bool, elem__121 bool) struct{} {
    vec_push__Vec_4bool(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__bool(self__131 *_goml_vec_bool) int32 {
    var retv97 int32
    var t98 int32 = vec_len__Vec_4bool(self__131)
    retv97 = t98
    return retv97
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv103 string
    var t104 string = _goml_runtime_core_int32_to_string(self__2)
    retv103 = t104
    return retv103
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv106 string
    var t107 string = _goml_runtime_core_bool_to_string(self__33)
    retv106 = t107
    return retv106
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv109 string
    retv109 = self__34
    return retv109
}

func main() {
    main0()
}
