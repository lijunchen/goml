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
    var retv28 string
    var t29 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t29)
    var t31 string = "items=" + t30
    retv28 = t31
    return retv28
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 2)
    var t33 string = render_all__T_int32(values__5)
    println__T_string(t33)
    var text__6 string = same__T_string__U_string("equal")
    println__T_string(text__6)
    var t34 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t35 string = _goml_m_require____T__Wrap_l_int32_r_(t34)
    println__T_string(t35)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__108 *_goml_vec_int32) int32 {
    var retv37 int32
    var t38 int32 = vec_len__Vec_5int32(self__108)
    retv37 = t38
    return retv37
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__97 *_goml_vec_int32, elem__98 int32) struct{} {
    vec_push__Vec_5int32(self__97, elem__98)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t45 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t45)
    return struct{}{}
}

func render_all__T_int32(values__2 *_goml_vec_int32) string {
    var retv48 string
    var t49 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__2)
    retv48 = t49
    return retv48
}

func same__T_string__U_string(value__3 string) string {
    var retv51 string
    retv51 = value__3
    return retv51
}

func _goml_m_require____T__Wrap_l_int32_r_(value__4 Wrap__int32) string {
    var retv53 string
    var t54 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(value__4)
    retv53 = t54
    return retv53
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv56 string
    retv56 = self__9
    return retv56
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    var retv58 string
    retv58 = "selected"
    return retv58
}

func main() {
    main0()
}
