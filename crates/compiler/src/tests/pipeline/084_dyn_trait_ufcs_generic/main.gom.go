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
    var retv60 string
    var t61 int32 = self__0.value
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t61)
    retv60 = t62
    return retv60
}

func render(x__2 dyn__Display) string {
    var retv64 string
    var t65 string = x__2.vtable.show(x__2.data)
    retv64 = t65
    return retv64
}

func main0() struct{} {
    var t67 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t67)
    var t68 string = render(d__3)
    println__T_string(t68)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int32_to_string(self__2)
    retv70 = t71
    return retv70
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var retv73 dyn__Display
    var t74 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    retv73 = t74
    return retv73
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv79 string
    retv79 = self__34
    return retv79
}

func main() {
    main0()
}
