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

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type GoError = error

func maybe_value__native(flag__0 bool) (int32, bool) {
    if flag__0 {
        return 4, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var retv11 int32
    var t12 int32 = a__1 + b__2
    retv11 = t12
    return retv11
}

func plus_two__native(flag__3 bool) (int32, bool) {
    var jp16 int32
    var mtmp0_value_0 int32
    var mtmp0_ok bool
    mtmp0_value_0, mtmp0_ok = maybe_value__native(flag__3)
    if !mtmp0_ok {
        var ret_zero int32
        return ret_zero, false
    }
    jp16 = mtmp0_value_0
    var t17 int32 = add(jp16, 2)
    return t17, true
}

func plus_two(flag__3 bool) Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = plus_two__native(flag__3)
    if native_ok {
        return Some{
            _0: native_value_0,
        }
    }
    return None{}
}

func show(opt__4 Option__int32) string {
    var retv20 string
    var jp22 string
    switch opt__4.(type) {
    case None:
        jp22 = "none"
    case Some:
        var x2 int32 = opt__4.(Some)._0
        var value__5 int32 = x2
        var t23 string = int32_to_string(value__5)
        var t24 string = "some=" + t23
        jp22 = t24
    default:
        panic("non-exhaustive match")
    }
    retv20 = jp22
    return retv20
}

func main0() struct{} {
    var t26 Option__int32 = plus_two(true)
    var t27 string = show(t26)
    println__T_string(t27)
    var t28 Option__int32 = plus_two(false)
    var t29 string = show(t28)
    println__T_string(t29)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
