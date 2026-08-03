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
    var t143 int
    var inline175 int = vec_len__Vec_5int32(self__0)
    t143 = inline175
    var t144 string
    var inline173 string = _goml_runtime_core_int_to_string(t143)
    t144 = inline173
    var t145 string = "items=" + t144
    return t145
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline195 int32 = 1
    vec_push__Vec_5int32(values__5, inline195)
    var inline192 int32 = 2
    vec_push__Vec_5int32(values__5, inline192)
    var t147 string
    var inline190 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__5)
    t147 = inline190
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline187)
    var text__6 string
    var inline185 string = "equal"
    text__6 = inline185
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline182)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t148 string
    var inline180 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(selected__7)
    t148 = inline180
    var inline177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline177)
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
