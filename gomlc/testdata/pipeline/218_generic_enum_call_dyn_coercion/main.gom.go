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
        var x155 int32 = self__0.(One)._0
        var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x155)
        return t160
    default:
        panic("non-exhaustive match")
    }
}

func render(value__3 dyn__Show) string {
    var t163 string = value__3.vtable.show(value__3.data)
    return t163
}

func main0() struct{} {
    var value__4 int32 = 42
    var t165 Boxed__int32 = make_boxed__T_int32(value__4)
    var t166 dyn__Show = dyn__Show{
        data: t165,
        vtable: dyn__Show__vtable__Boxed__int32(),
    }
    var t167 string = render(t166)
    println__T_string(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__6)
    return t171
}

func println__T_string(value__1 string) struct{} {
    var t173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func make_boxed__T_int32(value__2 int32) Boxed__int32 {
    var t177 Boxed__int32 = One{
        _0: value__2,
    }
    return t177
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
