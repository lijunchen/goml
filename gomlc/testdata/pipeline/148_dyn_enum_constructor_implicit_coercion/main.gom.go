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
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    return t162
}

func render(value__1 Boxed) string {
    switch value__1.(type) {
    case One:
        var x155 dyn__Display = value__1.(One)._0
        var t167 string = x155.vtable.show(x155.data)
        return t167
    case Pair:
        var x156 dyn__Display = value__1.(Pair)._0
        var x157 dyn__Display = value__1.(Pair)._1
        var t168 string = x156.vtable.show(x156.data)
        var t169 string = t168 + "-"
        var t170 string = x157.vtable.show(x157.data)
        var t171 string = t169 + t170
        return t171
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t173 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t174 Boxed = One{
        _0: t173,
    }
    var t175 string = render(t174)
    println__T_string(t175)
    var t176 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t177 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t178 Boxed = Pair{
        _0: t176,
        _1: t177,
    }
    var t179 string = render(t178)
    println__T_string(t179)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t182 string = _goml_runtime_core_int32_to_string(self__6)
    return t182
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
