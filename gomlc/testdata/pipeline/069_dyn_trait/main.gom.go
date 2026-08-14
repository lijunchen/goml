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
    var t186 int32 = self__0.x
    var t187 string
    var inline213 string = _goml_runtime_core_int32_to_string(t186)
    t187 = inline213
    var t188 string = "Point(" + t187
    var t189 string = t188 + ","
    var t190 int32 = self__0.y
    var t191 string
    var inline211 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline211
    var t192 string = t189 + t191
    var t193 string = t192 + ")"
    return t193
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var t198 bool = self__1.value
    if t198 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
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
    var t200 string = dp__4.vtable.show(dp__4.data)
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline218)
    var t201 string = dt__5.vtable.show(dt__5.data)
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
