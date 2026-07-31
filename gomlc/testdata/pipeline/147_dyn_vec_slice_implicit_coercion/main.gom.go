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
    var retv157 string
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv157 = t158
    return retv157
}

func render(x__1 dyn__Display) string {
    var retv160 string
    var t161 string = x__1.vtable.show(x__1.data)
    retv160 = t161
    return retv160
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t163 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t163)
    var t164 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__2, t164)
    var s__5 []dyn__Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(v__2, 0, 2)
    var t165 dyn__Display = s__5[0]
    var t166 string = render(t165)
    println__T_string(t166)
    var t__6 []dyn__Display = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(s__5, 1, 2)
    var t167 dyn__Display = t__6[0]
    var t168 string = render(t167)
    println__T_string(t168)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__6)
    retv170 = t171
    return retv170
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv173 *_goml_vec_Dyn_Display
    var t174 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__126 *_goml_vec_Dyn_Display, elem__127 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__dynDisplay(self__175 *_goml_vec_Dyn_Display, start__176 int, end__177 int) []dyn__Display {
    var retv178 []dyn__Display
    var t179 []dyn__Display = self__175.items[start__176:end__177]
    retv178 = t179
    return retv178
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_sub____T__dynDisplay(self__187 []dyn__Display, start__188 int, end__189 int) []dyn__Display {
    var retv184 []dyn__Display
    var t185 []dyn__Display = self__187[start__188:end__189]
    retv184 = t185
    return retv184
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv187 string
    retv187 = self__38
    return retv187
}

func main() {
    main0()
}
