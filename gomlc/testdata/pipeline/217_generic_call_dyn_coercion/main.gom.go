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
    var t184 int32 = self__0.value
    var inline206 string = _goml_runtime_core_int32_to_string(t184)
    return inline206
}

func main0() struct{} {
    var value__3 int32 = 42
    var t190 Wrap__int32
    var inline213 Wrap__int32 = Wrap__int32{
        value: value__3,
    }
    t190 = inline213
    var t191 dyn__Show = dyn__Show{
        data: t190,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t192 string
    var inline211 string = t191.vtable.show(t191.data)
    t192 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
