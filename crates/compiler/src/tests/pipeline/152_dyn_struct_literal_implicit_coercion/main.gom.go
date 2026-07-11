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
    var retv9 string
    var t10 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv9 = t10
    return retv9
}

func render(x__1 dyn__Display) string {
    var retv12 string
    var t13 string = x__1.vtable.show(x__1.data)
    retv12 = t13
    return retv12
}

func main0() struct{} {
    var t15 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__2 Holder__dynDisplay = Holder__dynDisplay{
        value: t15,
    }
    var t16 dyn__Display = holder__2.value
    var t17 string = render(t16)
    println__T_string(t17)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv19 string
    var t20 string = _goml_runtime_core_int32_to_string(self__2)
    retv19 = t20
    return retv19
}

func println__T_string(value__1 string) struct{} {
    var t22 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t22)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv25 string
    retv25 = self__9
    return retv25
}

func main() {
    main0()
}
