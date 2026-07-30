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
    var retv74 string
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv74 = t75
    return retv74
}

func render(value__1 Boxed) string {
    var retv77 string
    var jp79 string
    switch value__1.(type) {
    case One:
        var x68 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x68
        var t80 string = inner__2.vtable.show(inner__2.data)
        jp79 = t80
    case Pair:
        var x69 dyn__Display = value__1.(Pair)._0
        var x70 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x70
        var left__3 dyn__Display = x69
        var t81 string = left__3.vtable.show(left__3.data)
        var t82 string = t81 + "-"
        var t83 string = right__4.vtable.show(right__4.data)
        var t84 string = t82 + t83
        jp79 = t84
    default:
        panic("non-exhaustive match")
    }
    retv77 = jp79
    return retv77
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t86 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t87 Boxed = One{
        _0: t86,
    }
    var t88 string = render(t87)
    println__T_string(t88)
    var t89 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t90 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t91 Boxed = Pair{
        _0: t89,
        _1: t90,
    }
    var t92 string = render(t91)
    println__T_string(t92)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__6)
    retv94 = t95
    return retv94
}

func println__T_string(value__1 string) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv100 string
    retv100 = self__38
    return retv100
}

func main() {
    main0()
}
