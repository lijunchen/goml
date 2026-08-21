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

type Ordering int32

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
    return "point"
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var t419 int32 = self__1.value
    var t420 string
    var inline437 string = _goml_runtime_core_int32_to_string(t419)
    t420 = inline437
    var t421 string = prefix__2 + t420
    return t421
}

func main0() struct{} {
    var t427 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t427,
        vtable: dyn__Display__vtable__Point(),
    }
    var inline448 string = value__4.vtable.show(value__4.data, "value=")
    println__T_string(inline448)
    var inline450 string = value__4.vtable.name(value__4.data)
    println__T_string(inline450)
    var inline452 string = value__4.vtable.show(value__4.data, "again=")
    println__T_string(inline452)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t432 string
    t432 = value__1
    _goml_runtime_core_string_println(t432)
    return struct{}{}
}

func main() {
    main0()
}
