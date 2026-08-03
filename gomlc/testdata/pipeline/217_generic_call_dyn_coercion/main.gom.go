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

type Wrap__int32 struct {
    value int32
}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_Wrap____int32_i_show(self.(Wrap__int32))
}

func dyn__Show__vtable__Wrap__int32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_Wrap____int32_i_show(self__0 Wrap__int32) string {
    var t138 int32 = self__0.value
    var inline160 string = _goml_runtime_core_int32_to_string(t138)
    return inline160
}

func main0() struct{} {
    var value__3 int32 = 42
    var t144 Wrap__int32
    var inline167 Wrap__int32 = Wrap__int32{
        value: value__3,
    }
    t144 = inline167
    var t145 dyn__Show = dyn__Show{
        data: t144,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t146 string
    var inline165 string = t145.vtable.show(t145.data)
    t146 = inline165
    var inline162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline162)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
