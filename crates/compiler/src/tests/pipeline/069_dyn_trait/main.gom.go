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
    var retv7 string
    var t8 int32 = self__0.x
    var t9 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t8)
    var t10 string = "Point(" + t9
    var t11 string = t10 + ","
    var t12 int32 = self__0.y
    var t13 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t12)
    var t14 string = t11 + t13
    var t15 string = t14 + ")"
    retv7 = t15
    return retv7
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv17 string
    var t20 bool = self__1.value
    var jp19 string
    if t20 {
        jp19 = "Flag(true)"
    } else {
        jp19 = "Flag(false)"
    }
    retv17 = jp19
    return retv17
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
    var t22 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t22)
    var t23 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t23)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv25 string
    var t26 string = _goml_runtime_core_int32_to_string(self__2)
    retv25 = t26
    return retv25
}

func println__T_string(value__1 string) struct{} {
    var t28 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t28)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv31 string
    retv31 = self__9
    return retv31
}

func main() {
    main0()
}
