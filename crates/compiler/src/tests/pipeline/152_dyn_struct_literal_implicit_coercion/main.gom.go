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
    var retv6 string
    var t7 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv6 = t7
    return retv6
}

func render(x__1 dyn__Display) string {
    var retv9 string
    var t10 string = x__1.vtable.show(x__1.data)
    retv9 = t10
    return retv9
}

func main0() struct{} {
    var t12 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__2 Holder__dynDisplay = Holder__dynDisplay{
        value: t12,
    }
    var t13 dyn__Display = holder__2.value
    var t14 string = render(t13)
    println__T_string(t14)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv16 string
    var t17 string = _goml_runtime_core_int32_to_string(self__2)
    retv16 = t17
    return retv16
}

func println__T_string(value__1 string) struct{} {
    var t19 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t19)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv22 string
    retv22 = self__9
    return retv22
}

func main() {
    main0()
}
