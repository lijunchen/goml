package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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

type GoError = error

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_trait_impl_Display_int32_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_trait_impl_Display_int32_show(self__0 int32) string {
    var retv6 string
    var t7 string = int32_to_string(self__0)
    retv6 = t7
    return retv6
}

func render(value__1 Boxed) string {
    var retv9 string
    var jp11 string
    switch value__1.(type) {
    case One:
        var x0 dyn__Display = value__1.(One)._0
        var inner__2 dyn__Display = x0
        var t12 string = inner__2.vtable.show(inner__2.data)
        jp11 = t12
    case Pair:
        var x1 dyn__Display = value__1.(Pair)._0
        var x2 dyn__Display = value__1.(Pair)._1
        var right__4 dyn__Display = x2
        var left__3 dyn__Display = x1
        var t13 string = left__3.vtable.show(left__3.data)
        var t14 string = t13 + "-"
        var t15 string = right__4.vtable.show(right__4.data)
        var t16 string = t14 + t15
        jp11 = t16
    default:
        panic("non-exhaustive match")
    }
    retv9 = jp11
    return retv9
}

func main0() struct{} {
    var t18 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var t19 Boxed = One{
        _0: t18,
    }
    var t20 string = render(t19)
    println__T_string(t20)
    var t21 dyn__Display = dyn__Display{
        data: int32(7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t22 dyn__Display = dyn__Display{
        data: int32(9),
        vtable: dyn__Display__vtable__int32(),
    }
    var t23 Boxed = Pair{
        _0: t21,
        _1: t22,
    }
    var t24 string = render(t23)
    println__T_string(t24)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
