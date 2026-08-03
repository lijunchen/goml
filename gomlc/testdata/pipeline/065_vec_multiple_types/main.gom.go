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
    var inline242 int32 = 42
    vec_push__Vec_5int32(vi__0, inline242)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline240 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline240
    var vs__3 *_goml_vec_string
    var inline238 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline238
    var inline235 string = "hello"
    vec_push__Vec_6string(vs__3, inline235)
    var inline232 string = "world"
    vec_push__Vec_6string(vs__3, inline232)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline230 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline230
    var vb__6 *_goml_vec_bool
    var inline228 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline228
    var inline225 bool = true
    vec_push__Vec_4bool(vb__6, inline225)
    var inline222 bool = false
    vec_push__Vec_4bool(vb__6, inline222)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline220 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline220
    var t148 string
    var inline218 string = _goml_runtime_core_int32_to_string(val_i__1)
    t148 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline215)
    var t149 string
    var inline213 string = _goml_runtime_core_int_to_string(len_i__2)
    t149 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t149)
    _goml_runtime_core_string_println(inline210)
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline207)
    var t150 string
    var inline205 string = _goml_runtime_core_int_to_string(len_s__5)
    t150 = inline205
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline202)
    var t151 string
    var inline200 string = _goml_runtime_core_bool_to_string(val_b__7)
    t151 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t151)
    _goml_runtime_core_string_println(inline197)
    var t152 string
    var inline195 string = _goml_runtime_core_int_to_string(len_b__8)
    t152 = inline195
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
    _goml_runtime_core_string_println(inline192)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t155 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t155
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
