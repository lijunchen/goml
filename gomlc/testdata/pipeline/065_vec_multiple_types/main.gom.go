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
    var inline261 int32 = 42
    vec_push__Vec_5int32(vi__0, inline261)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline259 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline259
    var vs__3 *_goml_vec_string
    var inline257 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline257
    var inline254 string = "hello"
    vec_push__Vec_6string(vs__3, inline254)
    var inline251 string = "world"
    vec_push__Vec_6string(vs__3, inline251)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline249 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline249
    var vb__6 *_goml_vec_bool
    var inline247 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline247
    var inline244 bool = true
    vec_push__Vec_4bool(vb__6, inline244)
    var inline241 bool = false
    vec_push__Vec_4bool(vb__6, inline241)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline239 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline239
    var t167 string
    var inline237 string = _goml_runtime_core_int32_to_string(val_i__1)
    t167 = inline237
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline234)
    var t168 string
    var inline232 string = _goml_runtime_core_int_to_string(len_i__2)
    t168 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t168)
    _goml_runtime_core_string_println(inline229)
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline226)
    var t169 string
    var inline224 string = _goml_runtime_core_int_to_string(len_s__5)
    t169 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline221)
    var t170 string
    var inline219 string = _goml_runtime_core_bool_to_string(val_b__7)
    t170 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline216)
    var t171 string
    var inline214 string = _goml_runtime_core_int_to_string(len_b__8)
    t171 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t174 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
