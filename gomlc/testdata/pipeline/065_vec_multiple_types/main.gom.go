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
    var inline283 int32 = 42
    vec_push__Vec_5int32(vi__0, inline283)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline281 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline281
    var vs__3 *_goml_vec_string
    var inline279 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline279
    var inline276 string = "hello"
    vec_push__Vec_6string(vs__3, inline276)
    var inline273 string = "world"
    vec_push__Vec_6string(vs__3, inline273)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline271 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline271
    var vb__6 *_goml_vec_bool
    var inline269 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline269
    var inline266 bool = true
    vec_push__Vec_4bool(vb__6, inline266)
    var inline263 bool = false
    vec_push__Vec_4bool(vb__6, inline263)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline261 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline261
    var t189 string
    var inline259 string = _goml_runtime_core_int32_to_string(val_i__1)
    t189 = inline259
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline256)
    var t190 string
    var inline254 string = _goml_runtime_core_int_to_string(len_i__2)
    t190 = inline254
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline251)
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline248)
    var t191 string
    var inline246 string = _goml_runtime_core_int_to_string(len_s__5)
    t191 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline243)
    var t192 string
    var inline241 string = _goml_runtime_core_bool_to_string(val_b__7)
    t192 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline238)
    var t193 string
    var inline236 string = _goml_runtime_core_int_to_string(len_b__8)
    t193 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline233)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t196 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t196
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
