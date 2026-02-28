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

type Flag struct {
    value bool
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Flag__show(self any) string {
    return _goml_trait_impl_Display_Flag_show(self.(Flag))
}

func dyn__Display__vtable__Flag() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Flag__show,
    }
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
    var t2 int32
    var t3 string
    var t4 string
    var t5 string
    var t6 int32
    var t7 string
    var t8 string
    var t9 string
    t2 = self__0.x
    t3 = int32_to_string(t2)
    t4 = "Point(" + t3
    t5 = t4 + ","
    t6 = self__0.y
    t7 = int32_to_string(t6)
    t8 = t5 + t7
    t9 = t8 + ")"
    return t9
}

func _goml_trait_impl_Display_Flag_show(self__1 Flag) string {
    var t12 bool
    var jp11 string
    t12 = self__1.value
    if t12 {
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp11
    b2:
    jp11 = "Flag(true)"
    goto b1
    b3:
    jp11 = "Flag(false)"
    goto b1
}

func main0() struct{} {
    var p__2 Point
    var t__3 Flag
    var dp__4 dyn__Display
    var dt__5 dyn__Display
    var t13 string
    var t14 string
    p__2 = Point{
        x: 1,
        y: 2,
    }
    t__3 = Flag{
        value: true,
    }
    dp__4 = dyn__Display{
        data: p__2,
        vtable: dyn__Display__vtable__Point(),
    }
    dt__5 = dyn__Display{
        data: t__3,
        vtable: dyn__Display__vtable__Flag(),
    }
    t13 = dp__4.vtable.show(dp__4.data)
    string_println(t13)
    t14 = dt__5.vtable.show(dt__5.data)
    string_println(t14)
    return struct{}{}
}

func main() {
    main0()
}
