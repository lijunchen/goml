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
    var inline202 string = _goml_runtime_core_int32_to_string(self__0)
    return inline202
}

func main0() struct{} {
    var value__2 int32 = 42
    var t190 dyn__Display = dyn__Display{
        data: int32(value__2),
        vtable: dyn__Display__vtable__int32(),
    }
    var t192 string
    var inline207 string = t190.vtable.show(t190.data)
    t192 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
