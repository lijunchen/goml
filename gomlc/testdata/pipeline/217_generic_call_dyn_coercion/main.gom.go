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
    var retv69 string
    var t70 int32 = self__0.value
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t70)
    retv69 = t71
    return retv69
}

func render(value__2 dyn__Show) string {
    var retv73 string
    var t74 string = value__2.vtable.show(value__2.data)
    retv73 = t74
    return retv73
}

func main0() struct{} {
    var value__3 int32 = 42
    var t76 Wrap__int32 = make_wrap__T_int32(value__3)
    var t77 dyn__Show = dyn__Show{
        data: t76,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t78 string = render(t77)
    println__T_string(t78)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__6)
    retv81 = t82
    return retv81
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func make_wrap__T_int32(value__1 int32) Wrap__int32 {
    var retv87 Wrap__int32
    var t88 Wrap__int32 = Wrap__int32{
        value: value__1,
    }
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv90 string
    retv90 = self__38
    return retv90
}

func main() {
    main0()
}
