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

type Ordering int32

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
    var inline431 string = _goml_runtime_core_int32_to_string(self__0)
    return inline431
}

func main0() struct{} {
    var value__2 int32 = 42
    var t419 dyn__Display = dyn__Display{
        data: int32(value__2),
        vtable: dyn__Display__vtable__int32(),
    }
    var t421 string
    var inline436 string = t419.vtable.show(t419.data)
    t421 = inline436
    var inline433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline433)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
