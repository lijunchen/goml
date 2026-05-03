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

func maybe_value(flag__0 bool) Option__int32 {
    var retv6 Option__int32
    var jp8 Option__int32
    if flag__0 {
        var t9 Option__int32 = Some{
            _0: 4,
        }
        jp8 = t9
    } else {
        jp8 = None{}
    }
    retv6 = jp8
    return retv6
}

func add(a__1 int32, b__2 int32) int32 {
    var retv11 int32
    var t12 int32 = a__1 + b__2
    retv11 = t12
    return retv11
}

func plus_two(flag__3 bool) Option__int32 {
    var retv14 Option__int32
    var mtmp0 Option__int32 = maybe_value(flag__3)
    var jp16 int32
    switch mtmp0.(type) {
    case None:
        retv14 = None{}
        return retv14
    case Some:
        var x1 int32 = mtmp0.(Some)._0
        var try_value__15 int32 = x1
        jp16 = try_value__15
        var t17 int32 = add(jp16, 2)
        var t18 Option__int32 = Some{
            _0: t17,
        }
        retv14 = t18
        return retv14
    default:
        panic("non-exhaustive match")
    }
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
