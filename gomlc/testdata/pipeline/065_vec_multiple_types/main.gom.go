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
    var inline293 int32 = 42
    vec_push__Vec_5int32(vi__0, inline293)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline291 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline291
    var vs__3 *_goml_vec_string
    var inline289 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline289
    var inline286 string = "hello"
    vec_push__Vec_6string(vs__3, inline286)
    var inline283 string = "world"
    vec_push__Vec_6string(vs__3, inline283)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline281 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline281
    var vb__6 *_goml_vec_bool
    var inline279 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline279
    var inline276 bool = true
    vec_push__Vec_4bool(vb__6, inline276)
    var inline273 bool = false
    vec_push__Vec_4bool(vb__6, inline273)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline271 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline271
    var t199 string
    var inline269 string = _goml_runtime_core_int32_to_string(val_i__1)
    t199 = inline269
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline266)
    var t200 string
    var inline264 string = _goml_runtime_core_int_to_string(len_i__2)
    t200 = inline264
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline261)
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline258)
    var t201 string
    var inline256 string = _goml_runtime_core_int_to_string(len_s__5)
    t201 = inline256
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline253)
    var t202 string
    var inline251 string = _goml_runtime_core_bool_to_string(val_b__7)
    t202 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline248)
    var t203 string
    var inline246 string = _goml_runtime_core_int_to_string(len_b__8)
    t203 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline243)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t206 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t206
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
