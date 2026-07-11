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
    var retv28 string
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv28 = t29
    return retv28
}

func render(value__1 Boxed) string {
    var retv31 string
    var jp33 string
    switch value__1.(type) {
    case One:
        var x22 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x22
        var t34 string = inner__2.vtable.show(inner__2.data)
        jp33 = t34
    case Pair:
        var x23 dyn__Display = value__1.(Pair)._0
        var x24 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x24
        var left__3 dyn__Display = x23
        var t35 string = left__3.vtable.show(left__3.data)
        var t36 string = t35 + "-"
        var t37 string = right__4.vtable.show(right__4.data)
        var t38 string = t36 + t37
        jp33 = t38
    default:
        panic("non-exhaustive match")
    }
    retv31 = jp33
    return retv31
}

func main0() struct{} {
    var t40 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var t41 Boxed = One{
        _0: t40,
    }
    var t42 string = render(t41)
    println__T_string(t42)
    var t43 dyn__Display = dyn__Display{
        data: int32(7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t44 dyn__Display = dyn__Display{
        data: int32(9),
        vtable: dyn__Display__vtable__int32(),
    }
    var t45 Boxed = Pair{
        _0: t43,
        _1: t44,
    }
    var t46 string = render(t45)
    println__T_string(t46)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv48 string
    var t49 string = _goml_runtime_core_int32_to_string(self__2)
    retv48 = t49
    return retv48
}

func println__T_string(value__1 string) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func main() {
    main0()
}
