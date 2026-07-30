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
    var retv111 string
    var t112 int32 = self__0.x
    var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t112)
    var t114 string = "Point(" + t113
    var t115 string = t114 + ","
    var t116 int32 = self__0.y
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t116)
    var t118 string = t115 + t117
    var t119 string = t118 + ")"
    retv111 = t119
    return retv111
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv121 string
    var t124 bool = self__1.value
    var jp123 string
    if t124 {
        jp123 = "Flag(true)"
    } else {
        jp123 = "Flag(false)"
    }
    retv121 = jp123
    return retv121
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
    var t126 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t126)
    var t127 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t127)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int32_to_string(self__6)
    retv129 = t130
    return retv129
}

func println__T_string(value__1 string) struct{} {
    var t132 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t132)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv135 string
    retv135 = self__38
    return retv135
}

func main() {
    main0()
}
