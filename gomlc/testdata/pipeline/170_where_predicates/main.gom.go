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
    var t189 int
    var inline221 int = vec_len__Vec_5int32(self__0)
    t189 = inline221
    var t190 string
    var inline219 string = _goml_runtime_core_int_to_string(t189)
    t190 = inline219
    var t191 string = "items=" + t190
    return t191
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline241 int32 = 1
    vec_push__Vec_5int32(values__5, inline241)
    var inline238 int32 = 2
    vec_push__Vec_5int32(values__5, inline238)
    var t193 string
    var inline236 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__5)
    t193 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline233)
    var text__6 string
    var inline231 string = "equal"
    text__6 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline228)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t194 string
    var inline226 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(selected__7)
    t194 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline223)
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
