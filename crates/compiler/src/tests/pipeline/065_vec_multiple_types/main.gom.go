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
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(val_i__1)
    println__T_string(t34)
    var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_i__2)
    println__T_string(t35)
    println__T_string(val_s__4)
    var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_s__5)
    println__T_string(t36)
    var t37 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(val_b__7)
    println__T_string(t37)
    var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(len_b__8)
    println__T_string(t38)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv40 *_goml_vec_int32
    var t41 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv40 = t41
    return retv40
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__94 *_goml_vec_int32, elem__95 int32) struct{} {
    vec_push__Vec_5int32(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__105 *_goml_vec_int32) int32 {
    var retv45 int32
    var t46 int32 = vec_len__Vec_5int32(self__105)
    retv45 = t46
    return retv45
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv48 *_goml_vec_string
    var t49 *_goml_vec_string = vec_new__Vec_6string()
    retv48 = t49
    return retv48
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__94 *_goml_vec_string, elem__95 string) struct{} {
    vec_push__Vec_6string(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__105 *_goml_vec_string) int32 {
    var retv53 int32
    var t54 int32 = vec_len__Vec_6string(self__105)
    retv53 = t54
    return retv53
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__bool() *_goml_vec_bool {
    var retv56 *_goml_vec_bool
    var t57 *_goml_vec_bool = vec_new__Vec_4bool()
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(self__94 *_goml_vec_bool, elem__95 bool) struct{} {
    vec_push__Vec_4bool(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__bool(self__105 *_goml_vec_bool) int32 {
    var retv61 int32
    var t62 int32 = vec_len__Vec_4bool(self__105)
    retv61 = t62
    return retv61
}

func println__T_string(value__1 string) struct{} {
    var t64 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv67 string
    var t68 string = _goml_runtime_core_int32_to_string(self__2)
    retv67 = t68
    return retv67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv70 string
    var t71 string = _goml_runtime_core_bool_to_string(self__8)
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv73 string
    retv73 = self__9
    return retv73
}

func main() {
    main0()
}
