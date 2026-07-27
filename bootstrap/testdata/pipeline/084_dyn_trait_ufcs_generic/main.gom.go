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
    var retv66 string
    var t67 int32 = self__0.value
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t67)
    retv66 = t68
    return retv66
}

func render(x__2 dyn__Display) string {
    var retv70 string
    var t71 string = x__2.vtable.show(x__2.data)
    retv70 = t71
    return retv70
}

func main0() struct{} {
    var t73 Point = Point{
        value: 7,
    }
    var d__3 dyn__Display = to_dyn__T_Point(t73)
    var t74 string = render(d__3)
    println__T_string(t74)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__6)
    retv76 = t77
    return retv76
}

func to_dyn__T_Point(x__1 Point) dyn__Display {
    var retv79 dyn__Display
    var t80 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__Point(),
    }
    retv79 = t80
    return retv79
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv85 string
    retv85 = self__38
    return retv85
}

func main() {
    main0()
}
