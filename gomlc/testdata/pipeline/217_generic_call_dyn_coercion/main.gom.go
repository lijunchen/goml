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

type Wrap__i32 struct {
    value int32
}

type Ordering int32

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Wrap__i32__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_Wrap____i32_i_show(self.(Wrap__i32))
}

func dyn__Show__vtable__Wrap__i32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Wrap__i32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_Wrap____i32_i_show(self__0 Wrap__i32) string {
    var t413 int32 = self__0.value
    var inline435 string = _goml_runtime_core_int32_to_string(t413)
    return inline435
}

func main0() struct{} {
    var value__3 int32 = 42
    var t419 Wrap__i32
    var inline442 Wrap__i32 = Wrap__i32{
        value: value__3,
    }
    t419 = inline442
    var t420 dyn__Show = dyn__Show{
        data: t419,
        vtable: dyn__Show__vtable__Wrap__i32(),
    }
    var t421 string
    var inline440 string = t420.vtable.show(t420.data)
    t421 = inline440
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
