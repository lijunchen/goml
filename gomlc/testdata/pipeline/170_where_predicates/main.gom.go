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
    var t162 int
    var inline194 int = vec_len__Vec_5int32(self__0)
    t162 = inline194
    var t163 string
    var inline192 string = _goml_runtime_core_int_to_string(t162)
    t163 = inline192
    var t164 string = "items=" + t163
    return t164
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline214 int32 = 1
    vec_push__Vec_5int32(values__5, inline214)
    var inline211 int32 = 2
    vec_push__Vec_5int32(values__5, inline211)
    var t166 string
    var inline209 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__5)
    t166 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline206)
    var text__6 string
    var inline204 string = "equal"
    text__6 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline201)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t167 string
    var inline199 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(selected__7)
    t167 = inline199
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    return "selected"
}

func main() {
    main0()
}
