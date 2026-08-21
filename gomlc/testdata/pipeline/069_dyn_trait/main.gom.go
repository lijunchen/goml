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
    x int32
    y int32
}

type Flag struct {
    value bool
}

type Ordering int32

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Flag__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Flag_i_show(self.(Flag))
}

func dyn__Display__vtable__Flag() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Flag__show,
    }
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
    var t415 int32 = self__0.x
    var t416 string
    var inline442 string = _goml_runtime_core_int32_to_string(t415)
    t416 = inline442
    var t417 string = "Point(" + t416
    var t418 string = t417 + ","
    var t419 int32 = self__0.y
    var t420 string
    var inline440 string = _goml_runtime_core_int32_to_string(t419)
    t420 = inline440
    var t421 string = t418 + t420
    var t422 string = t421 + ")"
    return t422
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var t427 bool = self__1.value
    if t427 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
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
    var t429 string = dp__4.vtable.show(dp__4.data)
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline447)
    var t430 string = dt__5.vtable.show(dt__5.data)
    var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline444)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
