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
    var t184 int
    var inline216 int = vec_len__Vec_5int32(self__0)
    t184 = inline216
    var t185 string
    var inline214 string = _goml_runtime_core_int_to_string(t184)
    t185 = inline214
    var t186 string = "items=" + t185
    return t186
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline236 int32 = 1
    vec_push__Vec_5int32(values__5, inline236)
    var inline233 int32 = 2
    vec_push__Vec_5int32(values__5, inline233)
    var t188 string
    var inline231 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__5)
    t188 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline228)
    var text__6 string
    var inline226 string = "equal"
    text__6 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline223)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t189 string
    var inline221 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(selected__7)
    t189 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    return "selected"
}

func main() {
    main0()
}
