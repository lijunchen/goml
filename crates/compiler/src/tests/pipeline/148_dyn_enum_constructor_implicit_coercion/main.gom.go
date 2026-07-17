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
    var retv67 string
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv67 = t68
    return retv67
}

func render(value__1 Boxed) string {
    var retv70 string
    var jp72 string
    switch value__1.(type) {
    case One:
        var x61 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x61
        var t73 string = inner__2.vtable.show(inner__2.data)
        jp72 = t73
    case Pair:
        var x62 dyn__Display = value__1.(Pair)._0
        var x63 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x63
        var left__3 dyn__Display = x62
        var t74 string = left__3.vtable.show(left__3.data)
        var t75 string = t74 + "-"
        var t76 string = right__4.vtable.show(right__4.data)
        var t77 string = t75 + t76
        jp72 = t77
    default:
        panic("non-exhaustive match")
    }
    retv70 = jp72
    return retv70
}

func main0() struct{} {
    var t79 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var t80 Boxed = One{
        _0: t79,
    }
    var t81 string = render(t80)
    println__T_string(t81)
    var t82 dyn__Display = dyn__Display{
        data: int32(7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t83 dyn__Display = dyn__Display{
        data: int32(9),
        vtable: dyn__Display__vtable__int32(),
    }
    var t84 Boxed = Pair{
        _0: t82,
        _1: t83,
    }
    var t85 string = render(t84)
    println__T_string(t85)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__5)
    retv87 = t88
    return retv87
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv93 string
    retv93 = self__37
    return retv93
}

func main() {
    main0()
}
