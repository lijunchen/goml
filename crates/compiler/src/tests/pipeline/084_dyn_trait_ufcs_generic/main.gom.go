package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
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
    return _goml_trait_impl_Display_Point_show(self.(Point))
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
    }
}

func _goml_trait_impl_Display_Point_show(self__0 Point) string {
    var ret4 string
    var t1 int32 = self__0.value
    ret4 = int32_to_string(t1)
    return ret4
}

func render(x__2 dyn__Display) string {
    var ret5 string
    ret5 = x__2.vtable.show(x__2.data)
    return ret5
}

func main0() struct{} {
    var ret6 struct{}
    var t2 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t2)
    var t3 string = render(d__3)
    println__T_string(t3)
    ret6 = struct{}{}
    return ret6
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var ret7 dyn__Display
    ret7 = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    return ret7
}

func println__T_string(value__1 string) struct{} {
    var ret8 struct{}
    ret8 = string_println(value__1)
    return ret8
}

func main() {
    main0()
}
