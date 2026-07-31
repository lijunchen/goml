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

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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

func vec_get__Vec_4bool(vec *_goml_vec_bool, index int) bool {
    return vec.items[index]
}

func vec_len__Vec_4bool(vec *_goml_vec_bool) int {
    return int(len(vec.items))
}

func main0() struct{} {
    var vi__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(vi__0, 42)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(vi__0)
    var vs__3 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vs__3, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vs__3, "world")
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(vs__3)
    var vb__6 *_goml_vec_bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__bool()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(vb__6, true)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(vb__6, false)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__bool(vb__6)
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(val_i__1)
    println__T_string(t164)
    var t165 string = _goml_m_inherent_i_int_i_int_i_to__string(len_i__2)
    println__T_string(t165)
    println__T_string(val_s__4)
    var t166 string = _goml_m_inherent_i_int_i_int_i_to__string(len_s__5)
    println__T_string(t166)
    var t167 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(val_b__7)
    println__T_string(t167)
    var t168 string = _goml_m_inherent_i_int_i_int_i_to__string(len_b__8)
    println__T_string(t168)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv170 *_goml_vec_int32
    var t171 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv170 = t171
    return retv170
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv175 int
    var t176 int = vec_len__Vec_5int32(self__137)
    retv175 = t176
    return retv175
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv178 *_goml_vec_string
    var t179 *_goml_vec_string = vec_new__Vec_6string()
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv183 int
    var t184 int = vec_len__Vec_6string(self__137)
    retv183 = t184
    return retv183
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__bool() *_goml_vec_bool {
    var retv186 *_goml_vec_bool
    var t187 *_goml_vec_bool = vec_new__Vec_4bool()
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__bool(self__126 *_goml_vec_bool, elem__127 bool) struct{} {
    vec_push__Vec_4bool(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__bool(self__137 *_goml_vec_bool) int {
    var retv191 int
    var t192 int = vec_len__Vec_4bool(self__137)
    retv191 = t192
    return retv191
}

func println__T_string(value__1 string) struct{} {
    var t194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv197 string
    var t198 string = _goml_runtime_core_int32_to_string(self__6)
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv200 string
    var t201 string = _goml_runtime_core_int_to_string(self__5)
    retv200 = t201
    return retv200
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv203 string
    var t204 string = _goml_runtime_core_bool_to_string(self__37)
    retv203 = t204
    return retv203
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv206 string
    retv206 = self__38
    return retv206
}

func main() {
    main0()
}
