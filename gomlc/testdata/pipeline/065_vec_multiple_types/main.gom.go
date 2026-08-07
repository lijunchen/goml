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
    var inline278 int32 = 42
    vec_push__Vec_5int32(vi__0, inline278)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline276 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline276
    var vs__3 *_goml_vec_string
    var inline274 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline274
    var inline271 string = "hello"
    vec_push__Vec_6string(vs__3, inline271)
    var inline268 string = "world"
    vec_push__Vec_6string(vs__3, inline268)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline266 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline266
    var vb__6 *_goml_vec_bool
    var inline264 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline264
    var inline261 bool = true
    vec_push__Vec_4bool(vb__6, inline261)
    var inline258 bool = false
    vec_push__Vec_4bool(vb__6, inline258)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline256 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline256
    var t184 string
    var inline254 string = _goml_runtime_core_int32_to_string(val_i__1)
    t184 = inline254
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline251)
    var t185 string
    var inline249 string = _goml_runtime_core_int_to_string(len_i__2)
    t185 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline246)
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline243)
    var t186 string
    var inline241 string = _goml_runtime_core_int_to_string(len_s__5)
    t186 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline238)
    var t187 string
    var inline236 string = _goml_runtime_core_bool_to_string(val_b__7)
    t187 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline233)
    var t188 string
    var inline231 string = _goml_runtime_core_int_to_string(len_b__8)
    t188 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t191 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t191
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
