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
    var retv13 string
    var t14 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv13 = t14
    return retv13
}

func render(value__1 Boxed) string {
    var retv16 string
    var jp18 string
    switch value__1.(type) {
    case One:
        var x7 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x7
        var t19 string = inner__2.vtable.show(inner__2.data)
        jp18 = t19
    case Pair:
        var x8 dyn__Display = value__1.(Pair)._0
        var x9 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x9
        var left__3 dyn__Display = x8
        var t20 string = left__3.vtable.show(left__3.data)
        var t21 string = t20 + "-"
        var t22 string = right__4.vtable.show(right__4.data)
        var t23 string = t21 + t22
        jp18 = t23
    default:
        panic("non-exhaustive match")
    }
    retv16 = jp18
    return retv16
}

func main0() struct{} {
    var t25 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var t26 Boxed = One{
        _0: t25,
    }
    var t27 string = render(t26)
    println__T_string(t27)
    var t28 dyn__Display = dyn__Display{
        data: int32(7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t29 dyn__Display = dyn__Display{
        data: int32(9),
        vtable: dyn__Display__vtable__int32(),
    }
    var t30 Boxed = Pair{
        _0: t28,
        _1: t29,
    }
    var t31 string = render(t30)
    println__T_string(t31)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv33 string
    var t34 string = _goml_runtime_core_int32_to_string(self__2)
    retv33 = t34
    return retv33
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
