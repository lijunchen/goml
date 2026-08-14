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
    switch self__0.(type) {
    case One:
        var x187 int32 = self__0.(One)._0
        var inline210 string = _goml_runtime_core_int32_to_string(x187)
        return inline210
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var value__3 int32 = 42
    var t197 Boxed__int32 = One{
        _0: value__3,
    }
    var t198 dyn__Show = dyn__Show{
        data: t197,
        vtable: dyn__Show__vtable__Boxed__int32(),
    }
    var t199 string
    var inline215 string = t198.vtable.show(t198.data)
    t199 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
