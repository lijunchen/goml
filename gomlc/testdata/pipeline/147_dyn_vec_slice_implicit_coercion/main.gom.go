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
    var retv113 string
    var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv113 = t114
    return retv113
}

func render(x__1 dyn__Display) string {
    var retv116 string
    var t117 string = x__1.vtable.show(x__1.data)
    retv116 = t117
    return retv116
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t119 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t119)
    var t120 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t120)
    var s__5 []dyn__Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(v__2, 0, 2)
    var t121 dyn__Display = s__5[0]
    var t122 string = render(t121)
    println__T_string(t122)
    var t__6 []dyn__Display = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(s__5, 1, 2)
    var t123 dyn__Display = t__6[0]
    var t124 string = render(t123)
    println__T_string(t124)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__6)
    retv126 = t127
    return retv126
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv129 *_goml_vec_Dyn_Display
    var t130 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__126 *_goml_vec_Dyn_Display, elem__127 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(self__175 *_goml_vec_Dyn_Display, start__176 int, end__177 int) []dyn__Display {
    var retv134 []dyn__Display
    var t135 []dyn__Display = self__175.items[start__176:end__177]
    retv134 = t135
    return retv134
}

func println__T_string(value__1 string) struct{} {
    var t137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t137)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(self__187 []dyn__Display, start__188 int, end__189 int) []dyn__Display {
    var retv140 []dyn__Display
    var t141 []dyn__Display = self__187[start__188:end__189]
    retv140 = t141
    return retv140
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv143 string
    retv143 = self__38
    return retv143
}

func main() {
    main0()
}
