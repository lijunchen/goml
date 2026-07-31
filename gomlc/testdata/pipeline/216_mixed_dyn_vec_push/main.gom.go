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
    var retv156 string
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_Show_i_Wrap_i_show(self__1 Wrap) string {
    var retv159 string
    var t160 string = self__1.value
    retv159 = t160
    return retv159
}

func main0() struct{} {
    var values__2 *_goml_vec_Dyn_Show = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynShow()
    var value__3 int32 = 10
    var t162 dyn__Show = dyn__Show{
        data: int32(value__3),
        vtable: dyn__Show__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(values__2, t162)
    var t163 Wrap = Wrap{
        value: "ok",
    }
    var t164 dyn__Show = dyn__Show{
        data: t163,
        vtable: dyn__Show__vtable__Wrap(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(values__2, t164)
    var t165 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 0)
    var t166 string = t165.vtable.show(t165.data)
    println__T_string(t166)
    var t167 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 1)
    var t168 string = t167.vtable.show(t167.data)
    println__T_string(t168)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv171 string
    var t172 string = _goml_runtime_core_int32_to_string(self__6)
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynShow() *_goml_vec_Dyn_Show {
    var retv174 *_goml_vec_Dyn_Show
    var t175 *_goml_vec_Dyn_Show = vec_new__Vec_8Dyn_Show()
    retv174 = t175
    return retv174
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(self__126 *_goml_vec_Dyn_Show, elem__127 dyn__Show) struct{} {
    vec_push__Vec_8Dyn_Show(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv182 string
    retv182 = self__38
    return retv182
}

func main() {
    main0()
}
