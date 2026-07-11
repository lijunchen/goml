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
    var retv6 string
    var t7 int32 = self__0.value
    var t8 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t7)
    retv6 = t8
    return retv6
}

func render(x__2 dyn__Display) string {
    var retv10 string
    var t11 string = x__2.vtable.show(x__2.data)
    retv10 = t11
    return retv10
}

func main0() struct{} {
    var t13 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t13)
    var t14 string = render(d__3)
    println__T_string(t14)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv16 string
    var t17 string = _goml_runtime_core_int32_to_string(self__2)
    retv16 = t17
    return retv16
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var retv19 dyn__Display
    var t20 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    retv19 = t20
    return retv19
}

func println__T_string(value__1 string) struct{} {
    var t22 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t22)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv25 string
    retv25 = self__9
    return retv25
}

func main() {
    main0()
}
