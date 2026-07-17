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
    var retv63 string
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv63 = t64
    return retv63
}

func render(x__1 dyn__Display) string {
    var retv66 string
    var t67 string = x__1.vtable.show(x__1.data)
    retv66 = t67
    return retv66
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    var t69 dyn__Display = dyn__Display{
        data: int32(10),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t69)
    var t70 dyn__Display = dyn__Display{
        data: int32(20),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t70)
    var s__3 []dyn__Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(v__2, 0, 2)
    var t71 dyn__Display = s__3[0]
    var t72 string = render(t71)
    println__T_string(t72)
    var t__4 []dyn__Display = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(s__3, 1, 2)
    var t73 dyn__Display = t__4[0]
    var t74 string = render(t73)
    println__T_string(t74)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__2)
    retv76 = t77
    return retv76
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv79 *_goml_vec_Dyn_Display
    var t80 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv79 = t80
    return retv79
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__120 *_goml_vec_Dyn_Display, elem__121 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(self__169 *_goml_vec_Dyn_Display, start__170 int32, end__171 int32) []dyn__Display {
    var retv84 []dyn__Display
    var t85 []dyn__Display = self__169.items[start__170:end__171]
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(self__181 []dyn__Display, start__182 int32, end__183 int32) []dyn__Display {
    var retv90 []dyn__Display
    var t91 []dyn__Display = self__181[start__182:end__183]
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv93 string
    retv93 = self__34
    return retv93
}

func main() {
    main0()
}
