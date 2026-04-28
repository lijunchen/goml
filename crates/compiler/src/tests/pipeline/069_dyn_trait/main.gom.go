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
    x int32
    y int32
}

type Flag struct {
    value bool
}

type GoError = error

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Flag__show(self any) string {
    return _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_show(self.(Flag))
}

func dyn__Display__vtable__Flag() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Flag__show,
    }
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
    var retv3 string
    var t4 int32 = self__0.x
    var t5 string = int32_to_string(t4)
    var t6 string = "Point(" + t5
    var t7 string = t6 + ","
    var t8 int32 = self__0.y
    var t9 string = int32_to_string(t8)
    var t10 string = t7 + t9
    var t11 string = t10 + ")"
    retv3 = t11
    return retv3
}

func _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_show(self__1 Flag) string {
    var retv13 string
    var t16 bool = self__1.value
    var jp15 string
    if t16 {
        jp15 = "Flag(true)"
    } else {
        jp15 = "Flag(false)"
    }
    retv13 = jp15
    return retv13
}

func main0() struct{} {
    var p__2 Point = Point{
        x: 1,
        y: 2,
    }
    var t__3 Flag = Flag{
        value: true,
    }
    var dp__4 dyn__Display = dyn__Display{
        data: p__2,
        vtable: dyn__Display__vtable__Point(),
    }
    var dt__5 dyn__Display = dyn__Display{
        data: t__3,
        vtable: dyn__Display__vtable__Flag(),
    }
    var t18 string = dp__4.vtable.show(dp__4.data)
    string_println(t18)
    var t19 string = dt__5.vtable.show(dt__5.data)
    string_println(t19)
    return struct{}{}
}

func main() {
    main0()
}
