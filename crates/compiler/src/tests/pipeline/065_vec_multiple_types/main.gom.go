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
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(val_i__1)
    println__T_string(t73)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_i__2)
    println__T_string(t74)
    println__T_string(val_s__4)
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_s__5)
    println__T_string(t75)
    var t76 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(val_b__7)
    println__T_string(t76)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_b__8)
    println__T_string(t77)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv79 *_goml_vec_int32
    var t80 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv79 = t80
    return retv79
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv84 int32
    var t85 int32 = vec_len__Vec_5int32(self__134)
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv87 *_goml_vec_string
    var t88 *_goml_vec_string = vec_new__Vec_6string()
    retv87 = t88
    return retv87
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv92 int32
    var t93 int32 = vec_len__Vec_6string(self__134)
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__bool() *_goml_vec_bool {
    var retv95 *_goml_vec_bool
    var t96 *_goml_vec_bool = vec_new__Vec_4bool()
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(self__123 *_goml_vec_bool, elem__124 bool) struct{} {
    vec_push__Vec_4bool(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__bool(self__134 *_goml_vec_bool) int32 {
    var retv100 int32
    var t101 int32 = vec_len__Vec_4bool(self__134)
    retv100 = t101
    return retv100
}

func println__T_string(value__1 string) struct{} {
    var t103 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv106 string
    var t107 string = _goml_runtime_core_int32_to_string(self__5)
    retv106 = t107
    return retv106
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv109 string
    var t110 string = _goml_runtime_core_bool_to_string(self__36)
    retv109 = t110
    return retv109
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv112 string
    retv112 = self__37
    return retv112
}

func main() {
    main0()
}
