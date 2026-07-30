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
    var retv110 string
    var t111 int32 = self__0.value
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    retv110 = t112
    return retv110
}

func render(x__2 dyn__Display) string {
    var retv114 string
    var t115 string = x__2.vtable.show(x__2.data)
    retv114 = t115
    return retv114
}

func main0() struct{} {
    var t117 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t117)
    var t118 string = render(d__3)
    println__T_string(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__6)
    retv120 = t121
    return retv120
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var retv123 dyn__Display
    var t124 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    retv123 = t124
    return retv123
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv129 string
    retv129 = self__38
    return retv129
}

func main() {
    main0()
}
