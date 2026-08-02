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
    var retv158 string
    var t159 int32 = self__0.x
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t159)
    var t161 string = "Point(" + t160
    var t162 string = t161 + ","
    var t163 int32 = self__0.y
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t163)
    var t165 string = t162 + t164
    var t166 string = t165 + ")"
    retv158 = t166
    return retv158
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var retv168 string
    var t171 bool = self__1.value
    var jp170 string
    if t171 {
        jp170 = "Flag(true)"
    } else {
        jp170 = "Flag(false)"
    }
    retv168 = jp170
    return retv168
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
    var t173 string = dp__4.vtable.show(dp__4.data)
    println__T_string(t173)
    var t174 string = dt__5.vtable.show(dt__5.data)
    println__T_string(t174)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv176 string
    var t177 string = _goml_runtime_core_int32_to_string(self__6)
    retv176 = t177
    return retv176
}

func println__T_string(value__1 string) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv182 string
    retv182 = self__38
    return retv182
}

func main() {
    main0()
}
