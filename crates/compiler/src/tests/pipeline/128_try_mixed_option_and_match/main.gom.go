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

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func maybe_primary(flag__0 bool) Option__int32 {
    var retv9 Option__int32
    var jp11 Option__int32
    if flag__0 {
        var t12 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp11 = t12
    } else {
        jp11 = Option__int32_None{}
    }
    retv9 = jp11
    return retv9
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv14 Option__int32
    var jp16 Option__int32
    if flag__1 {
        var t17 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp16 = t17
    } else {
        jp16 = Option__int32_None{}
    }
    retv14 = jp16
    return retv14
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv19 Option__string
    var mtmp0 Option__int32 = maybe_primary(primary__2)
    var jp21 int32
    switch mtmp0.(type) {
    case Option__int32_None:
        retv19 = Option__string_None{}
        return retv19
    case Option__int32_Some:
        var x1 int32 = mtmp0.(Option__int32_Some)._0
        var try_value__18 int32 = x1
        jp21 = try_value__18
        var value__4 int32 = jp21
        var mtmp2 Option__int32 = maybe_secondary(secondary__3)
        var jp23 string
        switch mtmp2.(type) {
        case Option__int32_None:
            jp23 = "extra=none"
        case Option__int32_Some:
            var x3 int32 = mtmp2.(Option__int32_Some)._0
            var extra__5 int32 = x3
            var t29 string = int32_to_string(extra__5)
            var t30 string = "extra=" + t29
            jp23 = t30
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp23
        var t24 string = int32_to_string(value__4)
        var t25 string = "value=" + t24
        var t26 string = t25 + ","
        var t27 string = t26 + label__6
        var t28 Option__string = Option__string_Some{
            _0: t27,
        }
        retv19 = t28
        return retv19
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv32 string
    var jp34 string
    switch opt__7.(type) {
    case Option__string_None:
        jp34 = "none"
    case Option__string_Some:
        var x4 string = opt__7.(Option__string_Some)._0
        var value__8 string = x4
        var t35 string = "some=" + value__8
        jp34 = t35
    default:
        panic("non-exhaustive match")
    }
    retv32 = jp34
    return retv32
}

func main0() struct{} {
    var t37 Option__string = mixed(true, true)
    var t38 string = show(t37)
    println__T_string(t38)
    var t39 Option__string = mixed(true, false)
    var t40 string = show(t39)
    println__T_string(t40)
    var t41 Option__string = mixed(false, true)
    var t42 string = show(t41)
    println__T_string(t42)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
