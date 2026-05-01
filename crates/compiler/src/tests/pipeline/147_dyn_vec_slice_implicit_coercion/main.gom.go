package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_trait_x5f_impl_x23_Display_x23_int32_x23_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_trait_x5f_impl_x23_Display_x23_int32_x23_show(self__0 int32) string {
    var retv3 string
    var t4 string = int32_to_string(self__0)
    retv3 = t4
    return retv3
}

func render(x__1 dyn__Display) string {
    var retv6 string
    var t7 string = x__1.vtable.show(x__1.data)
    retv6 = t7
    return retv6
}

func main0() struct{} {
    var v__2 []dyn__Display = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_dynDisplay()
    var t9 dyn__Display = dyn__Display{
        data: int32(10),
        vtable: dyn__Display__vtable__int32(),
    }
    var v__3 []dyn__Display = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_dynDisplay(v__2, t9)
    var t10 dyn__Display = dyn__Display{
        data: int32(20),
        vtable: dyn__Display__vtable__int32(),
    }
    var v__4 []dyn__Display = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_dynDisplay(v__3, t10)
    var s__5 []dyn__Display = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_slice_x5f__x5f_T_x5f_dynDisplay(v__4, 0, 2)
    var t11 dyn__Display = s__5[0]
    var t12 string = render(t11)
    println__T_string(t12)
    var t__6 []dyn__Display = _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_sub_x5f__x5f_T_x5f_dynDisplay(s__5, 1, 2)
    var t13 dyn__Display = t__6[0]
    var t14 string = render(t13)
    println__T_string(t14)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_dynDisplay() []dyn__Display {
    var retv16 []dyn__Display
    var t17 []dyn__Display = nil
    retv16 = t17
    return retv16
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_dynDisplay(self__67 []dyn__Display, elem__68 dyn__Display) []dyn__Display {
    var retv19 []dyn__Display
    var t20 []dyn__Display = append(self__67, elem__68)
    retv19 = t20
    return retv19
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_slice_x5f__x5f_T_x5f_dynDisplay(self__75 []dyn__Display, start__76 int32, end__77 int32) []dyn__Display {
    var retv22 []dyn__Display
    var t23 []dyn__Display = self__75[start__76:end__77]
    retv22 = t23
    return retv22
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_Slice_x23_Slice_x5b_T_x5d__x23_sub_x5f__x5f_T_x5f_dynDisplay(self__81 []dyn__Display, start__82 int32, end__83 int32) []dyn__Display {
    var retv27 []dyn__Display
    var t28 []dyn__Display = self__81[start__82:end__83]
    retv27 = t28
    return retv27
}

func main() {
    main0()
}
