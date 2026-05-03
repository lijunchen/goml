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

func maybe_primary__native(flag__0 bool) (int32, bool) {
    if flag__0 {
        return 4, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func maybe_secondary__native(flag__1 bool) (int32, bool) {
    if flag__1 {
        return 9, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = maybe_secondary__native(flag__1)
    if native_ok {
        return Option__int32_Some{
            _0: native_value_0,
        }
    }
    return Option__int32_None{}
}

func mixed__native(primary__2 bool, secondary__3 bool) (string, bool) {
    var jp21 int32
    var mtmp0_value_0 int32
    var mtmp0_ok bool
    mtmp0_value_0, mtmp0_ok = maybe_primary__native(primary__2)
    if !mtmp0_ok {
        var ret_zero string
        return ret_zero, false
    }
    jp21 = mtmp0_value_0
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
    return t27, true
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var native_value_0 string
    var native_ok bool
    native_value_0, native_ok = mixed__native(primary__2, secondary__3)
    if native_ok {
        return Option__string_Some{
            _0: native_value_0,
        }
    }
    return Option__string_None{}
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
