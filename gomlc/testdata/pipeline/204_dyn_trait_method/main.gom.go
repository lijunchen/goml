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
    var retv157 string
    retv157 = "point"
    return retv157
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var retv159 string
    var t160 int32 = self__1.value
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t160)
    var t162 string = prefix__2 + t161
    retv159 = t162
    return retv159
}

func render(value__3 dyn__Display) struct{} {
    var t164 string = value__3.vtable.show(value__3.data, "value=")
    println__T_string(t164)
    var t165 string = value__3.vtable.name(value__3.data)
    println__T_string(t165)
    var t166 string = value__3.vtable.show(value__3.data, "again=")
    println__T_string(t166)
    return struct{}{}
}

func main0() struct{} {
    var t168 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t168,
        vtable: dyn__Display__vtable__Point(),
    }
    render(value__4)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__6)
    retv170 = t171
    return retv170
}

func println__T_string(value__1 string) struct{} {
    var t173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv176 string
    retv176 = self__38
    return retv176
}

func main() {
    main0()
}
