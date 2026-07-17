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
    var retv64 string
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv64 = t65
    return retv64
}

func render(value__1 Boxed) string {
    var retv67 string
    var jp69 string
    switch value__1.(type) {
    case One:
        var x58 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x58
        var t70 string = inner__2.vtable.show(inner__2.data)
        jp69 = t70
    case Pair:
        var x59 dyn__Display = value__1.(Pair)._0
        var x60 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x60
        var left__3 dyn__Display = x59
        var t71 string = left__3.vtable.show(left__3.data)
        var t72 string = t71 + "-"
        var t73 string = right__4.vtable.show(right__4.data)
        var t74 string = t72 + t73
        jp69 = t74
    default:
        panic("non-exhaustive match")
    }
    retv67 = jp69
    return retv67
}

func main0() struct{} {
    var t76 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var t77 Boxed = One{
        _0: t76,
    }
    var t78 string = render(t77)
    println__T_string(t78)
    var t79 dyn__Display = dyn__Display{
        data: int32(7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t80 dyn__Display = dyn__Display{
        data: int32(9),
        vtable: dyn__Display__vtable__int32(),
    }
    var t81 Boxed = Pair{
        _0: t79,
        _1: t80,
    }
    var t82 string = render(t81)
    println__T_string(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__2)
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv90 string
    retv90 = self__34
    return retv90
}

func main() {
    main0()
}
