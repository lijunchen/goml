package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type Wrap__int32 struct {
    value int32
}

func _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(self__0 *_goml_vec_int32) string {
    var t194 int
    var inline226 int = vec_len__Vec_5int32(self__0)
    t194 = inline226
    var t195 string
    var inline224 string = _goml_runtime_core_int_to_string(t194)
    t195 = inline224
    var t196 string = "items=" + t195
    return t196
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline246 int32 = 1
    vec_push__Vec_5int32(values__5, inline246)
    var inline243 int32 = 2
    vec_push__Vec_5int32(values__5, inline243)
    var t198 string
    var inline241 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__5)
    t198 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline238)
    var text__6 string
    var inline236 string = "equal"
    text__6 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline233)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t199 string
    var inline231 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(selected__7)
    t199 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    return "selected"
}

func main() {
    main0()
}
