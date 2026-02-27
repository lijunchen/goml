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
    var ret12 string
    var t6 int32 = self__0.x
    var t5 string = int32_to_string(t6)
    var t4 string = "Point(" + t5
    var t3 string = t4 + ","
    var t8 int32 = self__0.y
    var t7 string = int32_to_string(t8)
    var t2 string = t3 + t7
    ret12 = t2 + ")"
    return ret12
}

func _goml_trait_impl_Display_Flag_show(self__1 Flag) string {
    var ret13 string
    var t9 bool = self__1.value
    if t9 {
        ret13 = "Flag(true)"
    } else {
        ret13 = "Flag(false)"
    }
    return ret13
}

func main0() struct{} {
    var ret14 struct{}
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
    var t10 string = dp__4.vtable.show(dp__4.data)
    string_println(t10)
    var t11 string = dt__5.vtable.show(dt__5.data)
    string_println(t11)
    ret14 = struct{}{}
    return ret14
}

func main() {
    main0()
}
