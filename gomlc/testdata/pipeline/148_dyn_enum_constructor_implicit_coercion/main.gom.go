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
    var retv158 string
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv158 = t159
    return retv158
}

func render(value__1 Boxed) string {
    var retv161 string
    var jp163 string
    switch value__1.(type) {
    case One:
        var x152 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x152
        var t164 string = inner__2.vtable.show(inner__2.data)
        jp163 = t164
    case Pair:
        var x153 dyn__Display = value__1.(Pair)._0
        var x154 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x154
        var left__3 dyn__Display = x153
        var t165 string = left__3.vtable.show(left__3.data)
        var t166 string = t165 + "-"
        var t167 string = right__4.vtable.show(right__4.data)
        var t168 string = t166 + t167
        jp163 = t168
    default:
        panic("non-exhaustive match")
    }
    retv161 = jp163
    return retv161
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t170 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t171 Boxed = One{
        _0: t170,
    }
    var t172 string = render(t171)
    println__T_string(t172)
    var t173 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t174 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t175 Boxed = Pair{
        _0: t173,
        _1: t174,
    }
    var t176 string = render(t175)
    println__T_string(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int32_to_string(self__6)
    retv178 = t179
    return retv178
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func main() {
    main0()
}
