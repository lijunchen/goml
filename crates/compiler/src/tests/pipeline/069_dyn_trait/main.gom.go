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
    x int32
    y int32
}

type Flag struct {
    value bool
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Flag__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Flag_i_show(self.(Flag))
}

func dyn__Display__vtable__Flag() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Flag__show,
    }
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
    var retv64 string
    var t65 int32 = self__0.x
    var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t65)
    var t67 string = "Point(" + t66
    var t68 string = t67 + ","
    var t69 int32 = self__0.y
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    var t71 string = t68 + t70
    var t72 string = t71 + ")"
    retv64 = t72
    return retv64
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv74 string
    var t77 bool = self__1.value
    var jp76 string
    if t77 {
        jp76 = "Flag(true)"
    } else {
        jp76 = "Flag(false)"
    }
    retv74 = jp76
    return retv74
}

func main0() struct{} {
    var p__2 Point = Point{
        x: 1,
        y: 2,
    }
    var t__3 Flag = Flag{
        value: true,
    }
    var dp__4 dyn__Display = dyn__Display{
        data: p__2,
        vtable: dyn__Display__vtable__Point(),
    }
    var dt__5 dyn__Display = dyn__Display{
        data: t__3,
        vtable: dyn__Display__vtable__Flag(),
    }
    var t79 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t79)
    var t80 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t80)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int32_to_string(self__5)
    retv82 = t83
    return retv82
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv88 string
    retv88 = self__37
    return retv88
}

func main() {
    main0()
}
