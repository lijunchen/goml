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
    return "point"
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var t195 int32 = self__1.value
    var t196 string
    var inline213 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline213
    var t197 string = prefix__2 + t196
    return t197
}

func main0() struct{} {
    var t203 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t203,
        vtable: dyn__Display__vtable__Point(),
    }
    var inline224 string = value__4.vtable.show(value__4.data, "value=")
    println__T_string(inline224)
    var inline226 string = value__4.vtable.name(value__4.data)
    println__T_string(inline226)
    var inline228 string = value__4.vtable.show(value__4.data, "again=")
    println__T_string(inline228)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t208 string
    t208 = value__1
    _goml_runtime_core_string_println(t208)
    return struct{}{}
}

func main() {
    main0()
}
