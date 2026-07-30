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
    var retv109 string
    var t110 int32 = self__0.value
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t110)
    retv109 = t111
    return retv109
}

func render(value__2 dyn__Show) string {
    var retv113 string
    var t114 string = value__2.vtable.show(value__2.data)
    retv113 = t114
    return retv113
}

func main0() struct{} {
    var value__3 int32 = 42
    var t116 Wrap__int32 = make_wrap__T_int32(value__3)
    var t117 dyn__Show = dyn__Show{
        data: t116,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t118 string = render(t117)
    println__T_string(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int32_to_string(self__6)
    retv121 = t122
    return retv121
}

func println__T_string(value__1 string) struct{} {
    var t124 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t124)
    return struct{}{}
}

func make_wrap__T_int32(value__1 int32) Wrap__int32 {
    var retv127 Wrap__int32
    var t128 Wrap__int32 = Wrap__int32{
        value: value__1,
    }
    retv127 = t128
    return retv127
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv130 string
    retv130 = self__38
    return retv130
}

func main() {
    main0()
}
