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

type Holder__dynDisplay struct {
    value dyn__Display
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var retv24 string
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv24 = t25
    return retv24
}

func render(x__1 dyn__Display) string {
    var retv27 string
    var t28 string = x__1.vtable.show(x__1.data)
    retv27 = t28
    return retv27
}

func main0() struct{} {
    var t30 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__2 Holder__dynDisplay = Holder__dynDisplay{
        value: t30,
    }
    var t31 dyn__Display = holder__2.value
    var t32 string = render(t31)
    println__T_string(t32)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv34 string
    var t35 string = _goml_runtime_core_int32_to_string(self__2)
    retv34 = t35
    return retv34
}

func println__T_string(value__1 string) struct{} {
    var t37 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t37)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv40 string
    retv40 = self__9
    return retv40
}

func main() {
    main0()
}
