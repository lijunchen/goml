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
    var retv157 string
    var jp159 string
    switch self__0.(type) {
    case One:
        var x155 int32 = self__0.(One)._0
        var value__1 int32 = x155
        var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
        jp159 = t160
    default:
        panic("non-exhaustive match")
    }
    retv157 = jp159
    return retv157
}

func render(value__2 dyn__Show) string {
    var retv162 string
    var t163 string = value__2.vtable.show(value__2.data)
    retv162 = t163
    return retv162
}

func main0() struct{} {
    var value__3 int32 = 42
    var t165 Boxed__int32 = One{
        _0: value__3,
    }
    var t166 dyn__Show = dyn__Show{
        data: t165,
        vtable: dyn__Show__vtable__Boxed__int32(),
    }
    var t167 string = render(t166)
    println__T_string(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__6)
    retv170 = t171
    return retv170
}

func println__T_string(value__1 string) struct{} {
    var t173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv176 string
    retv176 = self__38
    return retv176
}

func main() {
    main0()
}
