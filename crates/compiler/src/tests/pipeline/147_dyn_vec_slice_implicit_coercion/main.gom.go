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
    var retv27 string
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv27 = t28
    return retv27
}

func render(x__1 dyn__Display) string {
    var retv30 string
    var t31 string = x__1.vtable.show(x__1.data)
    retv30 = t31
    return retv30
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    var t33 dyn__Display = dyn__Display{
        data: int32(10),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t33)
    var t34 dyn__Display = dyn__Display{
        data: int32(20),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t34)
    var s__3 []dyn__Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(v__2, 0, 2)
    var t35 dyn__Display = s__3[0]
    var t36 string = render(t35)
    println__T_string(t36)
    var t__4 []dyn__Display = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(s__3, 1, 2)
    var t37 dyn__Display = t__4[0]
    var t38 string = render(t37)
    println__T_string(t38)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv43 *_goml_vec_Dyn_Display
    var t44 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv43 = t44
    return retv43
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__97 *_goml_vec_Dyn_Display, elem__98 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__97, elem__98)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(self__109 *_goml_vec_Dyn_Display, start__110 int32, end__111 int32) []dyn__Display {
    var retv48 []dyn__Display
    var t49 []dyn__Display = self__109.items[start__110:end__111]
    retv48 = t49
    return retv48
}

func println__T_string(value__1 string) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(self__121 []dyn__Display, start__122 int32, end__123 int32) []dyn__Display {
    var retv54 []dyn__Display
    var t55 []dyn__Display = self__121[start__122:end__123]
    retv54 = t55
    return retv54
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func main() {
    main0()
}
