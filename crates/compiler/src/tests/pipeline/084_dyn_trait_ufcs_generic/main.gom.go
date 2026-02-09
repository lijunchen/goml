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
    x int32
    y int32
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
    var ret9 string
    var t5 int32 = self__0.x
    var t4 string = int32_to_string(t5)
    var t3 string = "P(" + t4
    var t2 string = t3 + ","
    var t7 int32 = self__0.y
    var t6 string = int32_to_string(t7)
    var t1 string = t2 + t6
    ret9 = t1 + ")"
    return ret9
}

func render(x__1 dyn__Display) string {
    var ret10 string
    ret10 = x__1.vtable.show(x__1.data)
    return ret10
}

func main0() struct{} {
    var ret11 struct{}
    var p__2 Point = Point{
        x: 7,
        y: 9,
    }
    var d__3 dyn__Display = dyn__Display{
        data: p__2,
        vtable: dyn__Display__vtable__Point(),
    }
    var t8 string = render(d__3)
    println__T_string(t8)
    ret11 = struct{}{}
    return ret11
}

func println__T_string(value__1 string) struct{} {
    var ret12 struct{}
    ret12 = string_println(value__1)
    return ret12
}

func main() {
    main0()
}
