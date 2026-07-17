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
    var retv64 string
    var t65 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t65)
    var t67 string = "items=" + t66
    retv64 = t67
    return retv64
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 2)
    var t69 string = render_all__T_int32(values__5)
    println__T_string(t69)
    var text__6 string = same__T_string__U_string("equal")
    println__T_string(text__6)
    var t70 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t71 string = _goml_m_require____T__Wrap_l_int32_r_(t70)
    println__T_string(t71)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv73 int32
    var t74 int32 = vec_len__Vec_5int32(self__131)
    retv73 = t74
    return retv73
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__2)
    retv76 = t77
    return retv76
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func render_all__T_int32(values__2 *_goml_vec_int32) string {
    var retv84 string
    var t85 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__2)
    retv84 = t85
    return retv84
}

func same__T_string__U_string(value__3 string) string {
    var retv87 string
    retv87 = value__3
    return retv87
}

func _goml_m_require____T__Wrap_l_int32_r_(value__4 Wrap__int32) string {
    var retv89 string
    var t90 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(value__4)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv92 string
    retv92 = self__34
    return retv92
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    var retv94 string
    retv94 = "selected"
    return retv94
}

func main() {
    main0()
}
