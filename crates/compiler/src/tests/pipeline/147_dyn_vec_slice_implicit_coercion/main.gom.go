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
    var retv66 string
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv66 = t67
    return retv66
}

func render(x__1 dyn__Display) string {
    var retv69 string
    var t70 string = x__1.vtable.show(x__1.data)
    retv69 = t70
    return retv69
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    var t72 dyn__Display = dyn__Display{
        data: int32(10),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t72)
    var t73 dyn__Display = dyn__Display{
        data: int32(20),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t73)
    var s__3 []dyn__Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(v__2, 0, 2)
    var t74 dyn__Display = s__3[0]
    var t75 string = render(t74)
    println__T_string(t75)
    var t__4 []dyn__Display = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(s__3, 1, 2)
    var t76 dyn__Display = t__4[0]
    var t77 string = render(t76)
    println__T_string(t77)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__5)
    retv79 = t80
    return retv79
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv82 *_goml_vec_Dyn_Display
    var t83 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv82 = t83
    return retv82
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__123 *_goml_vec_Dyn_Display, elem__124 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(self__172 *_goml_vec_Dyn_Display, start__173 int32, end__174 int32) []dyn__Display {
    var retv87 []dyn__Display
    var t88 []dyn__Display = self__172.items[start__173:end__174]
    retv87 = t88
    return retv87
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(self__184 []dyn__Display, start__185 int32, end__186 int32) []dyn__Display {
    var retv93 []dyn__Display
    var t94 []dyn__Display = self__184[start__185:end__186]
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv96 string
    retv96 = self__37
    return retv96
}

func main() {
    main0()
}
