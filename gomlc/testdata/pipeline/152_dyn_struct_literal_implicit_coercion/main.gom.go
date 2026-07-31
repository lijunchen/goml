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
    var retv154 string
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv154 = t155
    return retv154
}

func render(x__1 dyn__Display) string {
    var retv157 string
    var t158 string = x__1.vtable.show(x__1.data)
    retv157 = t158
    return retv157
}

func main0() struct{} {
    var value__2 int32 = 42
    var t160 dyn__Display = dyn__Display{
        data: int32(value__2),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__3 Holder__dynDisplay = Holder__dynDisplay{
        value: t160,
    }
    var t161 dyn__Display = holder__3.value
    var t162 string = render(t161)
    println__T_string(t162)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv164 string
    var t165 string = _goml_runtime_core_int32_to_string(self__6)
    retv164 = t165
    return retv164
}

func println__T_string(value__1 string) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv170 string
    retv170 = self__38
    return retv170
}

func main() {
    main0()
}
