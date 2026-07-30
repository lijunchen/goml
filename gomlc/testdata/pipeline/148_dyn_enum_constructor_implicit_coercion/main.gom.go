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
    var retv114 string
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv114 = t115
    return retv114
}

func render(value__1 Boxed) string {
    var retv117 string
    var jp119 string
    switch value__1.(type) {
    case One:
        var x108 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x108
        var t120 string = inner__2.vtable.show(inner__2.data)
        jp119 = t120
    case Pair:
        var x109 dyn__Display = value__1.(Pair)._0
        var x110 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x110
        var left__3 dyn__Display = x109
        var t121 string = left__3.vtable.show(left__3.data)
        var t122 string = t121 + "-"
        var t123 string = right__4.vtable.show(right__4.data)
        var t124 string = t122 + t123
        jp119 = t124
    default:
        panic("non-exhaustive match")
    }
    retv117 = jp119
    return retv117
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t126 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t127 Boxed = One{
        _0: t126,
    }
    var t128 string = render(t127)
    println__T_string(t128)
    var t129 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t130 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t131 Boxed = Pair{
        _0: t129,
        _1: t130,
    }
    var t132 string = render(t131)
    println__T_string(t132)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int32_to_string(self__6)
    retv134 = t135
    return retv134
}

func println__T_string(value__1 string) struct{} {
    var t137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t137)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv140 string
    retv140 = self__38
    return retv140
}

func main() {
    main0()
}
