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
    var retv66 string
    retv66 = "point"
    return retv66
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var retv68 string
    var t69 int32 = self__1.value
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    var t71 string = prefix__2 + t70
    retv68 = t71
    return retv68
}

func render(value__3 dyn__Display) struct{} {
    var t73 string = value__3.vtable.show(value__3.data, "value=")
    println__T_string(t73)
    var t74 string = value__3.vtable.name(value__3.data)
    println__T_string(t74)
    var t75 string = value__3.vtable.show(value__3.data, "again=")
    println__T_string(t75)
    return struct{}{}
}

func main0() struct{} {
    var t77 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t77,
        vtable: dyn__Display__vtable__Point(),
    }
    render(value__4)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__5)
    retv79 = t80
    return retv79
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv85 string
    retv85 = self__37
    return retv85
}

func main() {
    main0()
}
