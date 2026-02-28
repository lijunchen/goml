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
    var t1 int32 = self__0.value
    var t2 string = int32_to_string(t1)
    return t2
}

func render(x__2 dyn__Display) string {
    var t3 string = x__2.vtable.show(x__2.data)
    return t3
}

func main0() struct{} {
    var t4 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t4)
    var t5 string = render(d__3)
    println__T_string(t5)
    return struct{}{}
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var t6 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    return t6
}

func println__T_string(value__1 string) struct{} {
    var t7 struct{} = string_println(value__1)
    return t7
}

func main() {
    main0()
}
