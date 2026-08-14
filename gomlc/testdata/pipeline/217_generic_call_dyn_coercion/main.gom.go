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
    var t189 int32 = self__0.value
    var inline211 string = _goml_runtime_core_int32_to_string(t189)
    return inline211
}

func main0() struct{} {
    var value__3 int32 = 42
    var t195 Wrap__int32
    var inline218 Wrap__int32 = Wrap__int32{
        value: value__3,
    }
    t195 = inline218
    var t196 dyn__Show = dyn__Show{
        data: t195,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t197 string
    var inline216 string = t196.vtable.show(t196.data)
    t197 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline213)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
