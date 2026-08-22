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

type Wrap__i32 struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Render_i_Vec_l_i32_r__i_render(self__0 *_goml_vec_int32) string {
    var t418 int
    var inline450 int = vec_len__Vec_5int32(self__0)
    t418 = inline450
    var t419 string
    var inline448 string = _goml_runtime_core_int_to_string(t418)
    t419 = inline448
    var t420 string = "items=" + t419
    return t420
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline470 int32 = 1
    vec_push__Vec_5int32(values__5, inline470)
    var inline467 int32 = 2
    vec_push__Vec_5int32(values__5, inline467)
    var t422 string
    var inline465 string = _goml_m_trait__impl_i_Render_i_Vec_l_i32_r__i_render(values__5)
    t422 = inline465
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline462)
    var text__6 string
    var inline460 string = "equal"
    text__6 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline457)
    var selected__7 Wrap__i32 = Wrap__i32{
        value: 7,
    }
    var t423 string
    var inline455 string = _goml_m_trait__impl_i_Selected_i_Wrap____i32_i_selected(selected__7)
    t423 = inline455
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline452)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_Selected_i_Wrap____i32_i_selected(self__1 Wrap__i32) string {
    return "selected"
}

func main() {
    main0()
}
