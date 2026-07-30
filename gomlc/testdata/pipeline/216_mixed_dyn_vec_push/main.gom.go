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
    var retv72 string
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv72 = t73
    return retv72
}

func _goml_m_trait__impl_i_Show_i_Wrap_i_show(self__1 Wrap) string {
    var retv75 string
    var t76 string = self__1.value
    retv75 = t76
    return retv75
}

func main0() struct{} {
    var values__2 *_goml_vec_Dyn_Show = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynShow()
    var value__3 int32 = 10
    var t78 dyn__Show = dyn__Show{
        data: int32(value__3),
        vtable: dyn__Show__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(values__2, t78)
    var t79 Wrap = Wrap{
        value: "ok",
    }
    var t80 dyn__Show = dyn__Show{
        data: t79,
        vtable: dyn__Show__vtable__Wrap(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(values__2, t80)
    var t81 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 0)
    var t82 string = t81.vtable.show(t81.data)
    println__T_string(t82)
    var t83 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 1)
    var t84 string = t83.vtable.show(t83.data)
    println__T_string(t84)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__6)
    retv87 = t88
    return retv87
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynShow() *_goml_vec_Dyn_Show {
    var retv90 *_goml_vec_Dyn_Show
    var t91 *_goml_vec_Dyn_Show = vec_new__Vec_8Dyn_Show()
    retv90 = t91
    return retv90
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynShow(self__126 *_goml_vec_Dyn_Show, elem__127 dyn__Show) struct{} {
    vec_push__Vec_8Dyn_Show(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv98 string
    retv98 = self__38
    return retv98
}

func main() {
    main0()
}
