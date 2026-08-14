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
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
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
    var t185 int32 = self__0.value
    var inline205 string = _goml_runtime_core_int32_to_string(t185)
    return inline205
}

func main0() struct{} {
    var t191 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display
    var inline212 dyn__Display = dyn__Display{
        data: t191,
        vtable: dyn__Display__vtable__Point(),
    }
    d__3 = inline212
    var t192 string
    var inline210 string = d__3.vtable.show(d__3.data)
    t192 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
