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

type Boxed__int32 interface {
    isBoxed__int32()
}

type One struct {
    _0 int32
}

func (_ One) isBoxed__int32() {}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Boxed__int32__show(self any) string {
    switch v := self.(type) {
    case Boxed__int32:
        return _goml_m_trait__impl_i_Show_i_Boxed____int32_i_show(v)
    default:
        panic("unexpected type")
    }
}

func dyn__Show__vtable__Boxed__int32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Boxed__int32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_Boxed____int32_i_show(self__0 Boxed__int32) string {
    var retv110 string
    var jp112 string
    switch self__0.(type) {
    case One:
        var x108 int32 = self__0.(One)._0
        var value__1 int32 = x108
        var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
        jp112 = t113
    default:
        panic("non-exhaustive match")
    }
    retv110 = jp112
    return retv110
}

func render(value__3 dyn__Show) string {
    var retv115 string
    var t116 string = value__3.vtable.show(value__3.data)
    retv115 = t116
    return retv115
}

func main0() struct{} {
    var value__4 int32 = 42
    var t118 Boxed__int32 = make_boxed__T_int32(value__4)
    var t119 dyn__Show = dyn__Show{
        data: t118,
        vtable: dyn__Show__vtable__Boxed__int32(),
    }
    var t120 string = render(t119)
    println__T_string(t120)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int32_to_string(self__6)
    retv123 = t124
    return retv123
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func make_boxed__T_int32(value__2 int32) Boxed__int32 {
    var retv129 Boxed__int32
    var t130 Boxed__int32 = One{
        _0: value__2,
    }
    retv129 = t130
    return retv129
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv132 string
    retv132 = self__38
    return retv132
}

func main() {
    main0()
}
