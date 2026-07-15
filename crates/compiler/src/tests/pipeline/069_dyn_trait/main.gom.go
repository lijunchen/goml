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
    var retv25 string
    var t26 int32 = self__0.x
    var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t26)
    var t28 string = "Point(" + t27
    var t29 string = t28 + ","
    var t30 int32 = self__0.y
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t30)
    var t32 string = t29 + t31
    var t33 string = t32 + ")"
    retv25 = t33
    return retv25
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv35 string
    var t38 bool = self__1.value
    var jp37 string
    if t38 {
        jp37 = "Flag(true)"
    } else {
        jp37 = "Flag(false)"
    }
    retv35 = jp37
    return retv35
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
    var t40 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t40)
    var t41 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t41)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__2)
    retv43 = t44
    return retv43
}

func println__T_string(value__1 string) struct{} {
    var t46 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv49 string
    retv49 = self__9
    return retv49
}

func main() {
    main0()
}
