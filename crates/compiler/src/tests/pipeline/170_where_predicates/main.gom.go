package main

import (
    _goml_fmt "fmt"
)

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

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

type Wrap__int32 struct {
    value int32
}

func _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(self__0 *_goml_vec_int32) string {
    var retv67 string
    var t68 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t68)
    var t70 string = "items=" + t69
    retv67 = t70
    return retv67
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 2)
    var t72 string = render_all__T_int32(values__5)
    println__T_string(t72)
    var text__6 string = same__T_string__U_string("equal")
    println__T_string(text__6)
    var t73 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t74 string = _goml_m_require____T__Wrap_l_int32_r_(t73)
    println__T_string(t74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv76 int32
    var t77 int32 = vec_len__Vec_5int32(self__134)
    retv76 = t77
    return retv76
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__5)
    retv79 = t80
    return retv79
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func render_all__T_int32(values__2 *_goml_vec_int32) string {
    var retv87 string
    var t88 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__2)
    retv87 = t88
    return retv87
}

func same__T_string__U_string(value__3 string) string {
    var retv90 string
    retv90 = value__3
    return retv90
}

func _goml_m_require____T__Wrap_l_int32_r_(value__4 Wrap__int32) string {
    var retv92 string
    var t93 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(value__4)
    retv92 = t93
    return retv92
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv95 string
    retv95 = self__37
    return retv95
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    var retv97 string
    retv97 = "selected"
    return retv97
}

func main() {
    main0()
}
