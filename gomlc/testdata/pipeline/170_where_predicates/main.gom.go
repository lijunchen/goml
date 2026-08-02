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
    var retv161 string
    var t162 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__0)
    var t163 string = _goml_m_inherent_i_int_i_int_i_to__string(t162)
    var t164 string = "items=" + t163
    retv161 = t164
    return retv161
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__5, 2)
    var t166 string = render_all__T_int32(values__5)
    println__T_string(t166)
    var text__6 string = same__T_string__U_string("equal")
    println__T_string(text__6)
    var selected__7 Wrap__int32 = Wrap__int32{
        value: 7,
    }
    var t167 string = _goml_m_require____T__Wrap_l_int32_r_(selected__7)
    println__T_string(t167)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv169 int
    var t170 int = vec_len__Vec_5int32(self__137)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv172 string
    var t173 string = _goml_runtime_core_int_to_string(self__5)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t177)
    return struct{}{}
}

func render_all__T_int32(values__2 *_goml_vec_int32) string {
    var retv180 string
    var t181 string = _goml_m_trait__impl_i_Render_i_Vec_l_int32_r__i_render(values__2)
    retv180 = t181
    return retv180
}

func same__T_string__U_string(value__3 string) string {
    var retv183 string
    retv183 = value__3
    return retv183
}

func _goml_m_require____T__Wrap_l_int32_r_(value__4 Wrap__int32) string {
    var retv185 string
    var t186 string = _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(value__4)
    retv185 = t186
    return retv185
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv188 string
    retv188 = self__38
    return retv188
}

func _goml_m_trait__impl_i_Selected_i_Wrap____int32_i_selected(self__1 Wrap__int32) string {
    var retv190 string
    retv190 = "selected"
    return retv190
}

func main() {
    main0()
}
