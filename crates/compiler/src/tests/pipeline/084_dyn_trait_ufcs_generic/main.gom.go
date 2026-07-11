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

type Point struct {
    value int32
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Point__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Point_i_show(self.(Point))
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
    }
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__0 Point) string {
    var retv9 string
    var t10 int32 = self__0.value
    var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t10)
    retv9 = t11
    return retv9
}

func render(x__2 dyn__Display) string {
    var retv13 string
    var t14 string = x__2.vtable.show(x__2.data)
    retv13 = t14
    return retv13
}

func main0() struct{} {
    var t16 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t16)
    var t17 string = render(d__3)
    println__T_string(t17)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv19 string
    var t20 string = _goml_runtime_core_int32_to_string(self__2)
    retv19 = t20
    return retv19
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var retv22 dyn__Display
    var t23 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    retv22 = t23
    return retv22
}

func println__T_string(value__1 string) struct{} {
    var t25 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t25)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv28 string
    retv28 = self__9
    return retv28
}

func main() {
    main0()
}
