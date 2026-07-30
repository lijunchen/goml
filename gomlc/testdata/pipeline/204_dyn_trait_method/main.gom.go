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
    value int32
}

type dyn__Display_vtable struct {
    show func(any, string) string
    name func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Point__show(self any, p0 string) string {
    return _goml_m_trait__impl_i_Display_i_Point_i_show(self.(Point), p0)
}

func dyn__Display__wrap__Point__name(self any) string {
    return _goml_m_trait__impl_i_Named_i_Point_i_name(self.(Point))
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
        name: dyn__Display__wrap__Point__name,
    }
}

func _goml_m_trait__impl_i_Named_i_Point_i_name(self__0 Point) string {
    var retv113 string
    retv113 = "point"
    return retv113
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var retv115 string
    var t116 int32 = self__1.value
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t116)
    var t118 string = prefix__2 + t117
    retv115 = t118
    return retv115
}

func render(value__3 dyn__Display) struct{} {
    var t120 string = value__3.vtable.show(value__3.data, "value=")
    println__T_string(t120)
    var t121 string = value__3.vtable.name(value__3.data)
    println__T_string(t121)
    var t122 string = value__3.vtable.show(value__3.data, "again=")
    println__T_string(t122)
    return struct{}{}
}

func main0() struct{} {
    var t124 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t124,
        vtable: dyn__Display__vtable__Point(),
    }
    render(value__4)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__6)
    retv126 = t127
    return retv126
}

func println__T_string(value__1 string) struct{} {
    var t129 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t129)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv132 string
    retv132 = self__38
    return retv132
}

func main() {
    main0()
}
