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
    var retv71 string
    var t72 int32 = self__0.x
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    var t74 string = "Point(" + t73
    var t75 string = t74 + ","
    var t76 int32 = self__0.y
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    var t78 string = t75 + t77
    var t79 string = t78 + ")"
    retv71 = t79
    return retv71
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv81 string
    var t84 bool = self__1.value
    var jp83 string
    if t84 {
        jp83 = "Flag(true)"
    } else {
        jp83 = "Flag(false)"
    }
    retv81 = jp83
    return retv81
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
    var t86 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t86)
    var t87 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t87)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__6)
    retv89 = t90
    return retv89
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv95 string
    retv95 = self__38
    return retv95
}

func main() {
    main0()
}
