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
    var retv70 string
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv70 = t71
    return retv70
}

func render(value__1 Boxed) string {
    var retv73 string
    var jp75 string
    switch value__1.(type) {
    case One:
        var x64 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x64
        var t76 string = inner__2.vtable.show(inner__2.data)
        jp75 = t76
    case Pair:
        var x65 dyn__Display = value__1.(Pair)._0
        var x66 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x66
        var left__3 dyn__Display = x65
        var t77 string = left__3.vtable.show(left__3.data)
        var t78 string = t77 + "-"
        var t79 string = right__4.vtable.show(right__4.data)
        var t80 string = t78 + t79
        jp75 = t80
    default:
        panic("non-exhaustive match")
    }
    retv73 = jp75
    return retv73
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t82 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t83 Boxed = One{
        _0: t82,
    }
    var t84 string = render(t83)
    println__T_string(t84)
    var t85 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t86 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t87 Boxed = Pair{
        _0: t85,
        _1: t86,
    }
    var t88 string = render(t87)
    println__T_string(t88)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__6)
    retv90 = t91
    return retv90
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv96 string
    retv96 = self__38
    return retv96
}

func main() {
    main0()
}
