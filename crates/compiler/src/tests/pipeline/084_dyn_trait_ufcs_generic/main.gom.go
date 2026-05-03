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
    return _goml_trait_x5f_impl_x23_Display_x23_Point_x23_show(self.(Point))
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
    }
}

func _goml_trait_x5f_impl_x23_Display_x23_Point_x23_show(self__0 Point) string {
    var retv2 string
    var t3 int32 = self__0.value
    var t4 string = int32_to_string(t3)
    retv2 = t4
    return retv2
}

func render(x__2 dyn__Display) string {
    var retv6 string
    var t7 string = x__2.vtable.show(x__2.data)
    retv6 = t7
    return retv6
}

func main0() struct{} {
    var t9 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t9)
    var t10 string = render(d__3)
    println__T_string(t10)
    return struct{}{}
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var retv12 dyn__Display
    var t13 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    retv12 = t13
    return retv12
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
