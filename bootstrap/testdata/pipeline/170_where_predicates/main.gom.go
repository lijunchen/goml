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
    var retv70 string
    var t71 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    var t72 string = _goml_m_inherent_i_int_i_int_i_to__string(t71)
    var t73 string = "items=" + t72
    retv70 = t73
    return retv70
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 2)
    var t75 string = render_all__T_int32(values__5)
    println__T_string(t75)
    var text__6 string = same__T_string__U_string("equal")
    println__T_string(text__6)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t76 string = _goml_m_require____T__Wrap_l_int32_r_(selected__7)
    println__T_string(t76)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv78 int
    var t79 int = vec_len__Vec_5int32(self__139)
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int_to_string(self__5)
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func render_all__T_int32(values__2 *_goml_vec_int32) string {
    var retv89 string
    var t90 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__2)
    retv89 = t90
    return retv89
}

func same__T_string__U_string(value__3 string) string {
    var retv92 string
    retv92 = value__3
    return retv92
}

func _goml_m_require____T__Wrap_l_int32_r_(value__4 Wrap__int32) string {
    var retv94 string
    var t95 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(value__4)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv97 string
    retv97 = self__38
    return retv97
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    var retv99 string
    retv99 = "selected"
    return retv99
}

func main() {
    main0()
}
