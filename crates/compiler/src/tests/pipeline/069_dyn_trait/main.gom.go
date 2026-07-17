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
    var retv61 string
    var t62 int32 = self__0.x
    var t63 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t62)
    var t64 string = "Point(" + t63
    var t65 string = t64 + ","
    var t66 int32 = self__0.y
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    var t68 string = t65 + t67
    var t69 string = t68 + ")"
    retv61 = t69
    return retv61
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv71 string
    var t74 bool = self__1.value
    var jp73 string
    if t74 {
        jp73 = "Flag(true)"
    } else {
        jp73 = "Flag(false)"
    }
    retv71 = jp73
    return retv71
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
    var t76 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t76)
    var t77 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t77)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__2)
    retv79 = t80
    return retv79
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv85 string
    retv85 = self__34
    return retv85
}

func main() {
    main0()
}
