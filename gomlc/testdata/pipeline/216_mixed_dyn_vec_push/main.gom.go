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

type _goml_vec_Dyn_Show struct {
    items []dyn__Show
}

func vec_new__Vec_8Dyn_Show() *_goml_vec_Dyn_Show {
    return &_goml_vec_Dyn_Show{
        items: nil,
    }
}

func vec_push__Vec_8Dyn_Show(vec *_goml_vec_Dyn_Show, elem dyn__Show) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_8Dyn_Show(vec *_goml_vec_Dyn_Show, index int) dyn__Show {
    return vec.items[index]
}

type Wrap struct {
    value string
}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Wrap__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_Wrap_i_show(self.(Wrap))
}

func dyn__Show__vtable__Wrap() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Wrap__show,
    }
}

func dyn__Show__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_int32_i_show(self.(int32))
}

func dyn__Show__vtable__int32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_int32_i_show(self__0 int32) string {
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    return t160
}

func _goml_m_trait__impl_i_Show_i_Wrap_i_show(self__1 Wrap) string {
    var t163 string = self__1.value
    return t163
}

func main0() struct{} {
    var values__2 *_goml_vec_Dyn_Show = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynShow()
    var value__3 int32 = 10
    var t165 dyn__Show = dyn__Show{
        data: int32(value__3),
        vtable: dyn__Show__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(values__2, t165)
    var t166 Wrap = Wrap{
        value: "ok",
    }
    var t167 dyn__Show = dyn__Show{
        data: t166,
        vtable: dyn__Show__vtable__Wrap(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(values__2, t167)
    var t168 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 0)
    var t169 string = t168.vtable.show(t168.data)
    println__T_string(t169)
    var t170 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 1)
    var t171 string = t170.vtable.show(t170.data)
    println__T_string(t171)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t175 string = _goml_runtime_core_int32_to_string(self__6)
    return t175
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynShow() *_goml_vec_Dyn_Show {
    var t178 *_goml_vec_Dyn_Show = vec_new__Vec_8Dyn_Show()
    return t178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(self__126 *_goml_vec_Dyn_Show, elem__127 dyn__Show) struct{} {
    vec_push__Vec_8Dyn_Show(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
