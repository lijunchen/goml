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
    var ret13 string
    var t7 int32 = self__0.x
    var t6 string = int32_to_string(t7)
    var t5 string = "Point(" + t6
    var t4 string = t5 + ","
    var t9 int32 = self__0.y
    var t8 string = int32_to_string(t9)
    var t3 string = t4 + t8
    ret13 = t3 + ")"
    return ret13
}

func _goml_trait_impl_Display_Flag_show(self__1 Flag) string {
    var ret14 string
    var t10 bool = self__1.value
    if t10 {
        ret14 = "Flag(true)"
    } else {
        ret14 = "Flag(false)"
    }
    return ret14
}

func main0() struct{} {
    var ret15 struct{}
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
    var t11 string = dp__4.vtable.show(dp__4.data)
    string_println(t11)
    var t12 string = dt__5.vtable.show(dt__5.data)
    string_println(t12)
    ret15 = struct{}{}
    return ret15
}

func main() {
    main0()
}
