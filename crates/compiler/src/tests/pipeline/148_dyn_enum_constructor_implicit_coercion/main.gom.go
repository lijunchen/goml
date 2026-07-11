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

type Boxed interface {
    isBoxed()
}

type One struct {
    _0 dyn__Display
}

func (_ One) isBoxed() {}

type Pair struct {
    _0 dyn__Display
    _1 dyn__Display
}

func (_ Pair) isBoxed() {}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var retv10 string
    var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv10 = t11
    return retv10
}

func render(value__1 Boxed) string {
    var retv13 string
    var jp15 string
    switch value__1.(type) {
    case One:
        var x4 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x4
        var t16 string = inner__2.vtable.show(inner__2.data)
        jp15 = t16
    case Pair:
        var x5 dyn__Display = value__1.(Pair)._0
        var x6 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x6
        var left__3 dyn__Display = x5
        var t17 string = left__3.vtable.show(left__3.data)
        var t18 string = t17 + "-"
        var t19 string = right__4.vtable.show(right__4.data)
        var t20 string = t18 + t19
        jp15 = t20
    default:
        panic("non-exhaustive match")
    }
    retv13 = jp15
    return retv13
}

func main0() struct{} {
    var t22 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var t23 Boxed = One{
        _0: t22,
    }
    var t24 string = render(t23)
    println__T_string(t24)
    var t25 dyn__Display = dyn__Display{
        data: int32(7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t26 dyn__Display = dyn__Display{
        data: int32(9),
        vtable: dyn__Display__vtable__int32(),
    }
    var t27 Boxed = Pair{
        _0: t25,
        _1: t26,
    }
    var t28 string = render(t27)
    println__T_string(t28)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int32_to_string(self__2)
    retv30 = t31
    return retv30
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv36 string
    retv36 = self__9
    return retv36
}

func main() {
    main0()
}
