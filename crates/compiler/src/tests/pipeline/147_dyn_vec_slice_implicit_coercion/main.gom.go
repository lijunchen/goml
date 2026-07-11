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

type _goml_vec_Dyn_Display struct {
    items []dyn__Display
}

func vec_new__Vec_11Dyn_Display() *_goml_vec_Dyn_Display {
    return &_goml_vec_Dyn_Display{
        items: nil,
    }
}

func vec_push__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display, elem dyn__Display) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var retv9 string
    var t10 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv9 = t10
    return retv9
}

func render(x__1 dyn__Display) string {
    var retv12 string
    var t13 string = x__1.vtable.show(x__1.data)
    retv12 = t13
    return retv12
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    var t15 dyn__Display = dyn__Display{
        data: int32(10),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t15)
    var t16 dyn__Display = dyn__Display{
        data: int32(20),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t16)
    var s__3 []dyn__Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(v__2, 0, 2)
    var t17 dyn__Display = s__3[0]
    var t18 string = render(t17)
    println__T_string(t18)
    var t__4 []dyn__Display = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(s__3, 1, 2)
    var t19 dyn__Display = t__4[0]
    var t20 string = render(t19)
    println__T_string(t20)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv22 string
    var t23 string = _goml_runtime_core_int32_to_string(self__2)
    retv22 = t23
    return retv22
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv25 *_goml_vec_Dyn_Display
    var t26 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv25 = t26
    return retv25
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__71 *_goml_vec_Dyn_Display, elem__72 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(self__83 *_goml_vec_Dyn_Display, start__84 int32, end__85 int32) []dyn__Display {
    var retv30 []dyn__Display
    var t31 []dyn__Display = self__83.items[start__84:end__85]
    retv30 = t31
    return retv30
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(self__89 []dyn__Display, start__90 int32, end__91 int32) []dyn__Display {
    var retv36 []dyn__Display
    var t37 []dyn__Display = self__89[start__90:end__91]
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
