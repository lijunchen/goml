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
    var t157 int32 = self__0.value
    var inline179 string = _goml_runtime_core_int32_to_string(t157)
    return inline179
}

func main0() struct{} {
    var value__3 int32 = 42
    var t163 Wrap__int32
    var inline186 Wrap__int32 = Wrap__int32{
        value: value__3,
    }
    t163 = inline186
    var t164 dyn__Show = dyn__Show{
        data: t163,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t165 string
    var inline184 string = t164.vtable.show(t164.data)
    t165 = inline184
    var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
