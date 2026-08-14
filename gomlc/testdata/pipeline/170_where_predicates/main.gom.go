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

type Ordering int32

func _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(self__0 *_goml_vec_int32) string {
    var t415 int
    var inline447 int = vec_len__Vec_5int32(self__0)
    t415 = inline447
    var t416 string
    var inline445 string = _goml_runtime_core_int_to_string(t415)
    t416 = inline445
    var t417 string = "items=" + t416
    return t417
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline467 int32 = 1
    vec_push__Vec_5int32(values__5, inline467)
    var inline464 int32 = 2
    vec_push__Vec_5int32(values__5, inline464)
    var t419 string
    var inline462 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__5)
    t419 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline459)
    var text__6 string
    var inline457 string = "equal"
    text__6 = inline457
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline454)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t420 string
    var inline452 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(selected__7)
    t420 = inline452
    var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline449)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    return "selected"
}

func main() {
    main0()
}
