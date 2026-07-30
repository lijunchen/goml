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
    var retv73 string
    retv73 = "point"
    return retv73
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var retv75 string
    var t76 int32 = self__1.value
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    var t78 string = prefix__2 + t77
    retv75 = t78
    return retv75
}

func render(value__3 dyn__Display) struct{} {
    var t80 string = value__3.vtable.show(value__3.data, "value=")
    println__T_string(t80)
    var t81 string = value__3.vtable.name(value__3.data)
    println__T_string(t81)
    var t82 string = value__3.vtable.show(value__3.data, "again=")
    println__T_string(t82)
    return struct{}{}
}

func main0() struct{} {
    var t84 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t84,
        vtable: dyn__Display__vtable__Point(),
    }
    render(value__4)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv86 string
    var t87 string = _goml_runtime_core_int32_to_string(self__6)
    retv86 = t87
    return retv86
}

func println__T_string(value__1 string) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv92 string
    retv92 = self__38
    return retv92
}

func main() {
    main0()
}
