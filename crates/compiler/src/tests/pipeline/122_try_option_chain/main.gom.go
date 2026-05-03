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

func maybe_seed(flag__0 bool) Option__int32 {
    var retv8 Option__int32
    var jp10 Option__int32
    if flag__0 {
        var t11 Option__int32 = Some{
            _0: 3,
        }
        jp10 = t11
    } else {
        jp10 = None{}
    }
    retv8 = jp10
    return retv8
}

func maybe_double(value__1 int32) Option__int32 {
    var retv13 Option__int32
    var t16 bool = value__1 > 0
    var jp15 Option__int32
    if t16 {
        var t17 int32 = value__1 * 2
        var t18 Option__int32 = Some{
            _0: t17,
        }
        jp15 = t18
    } else {
        jp15 = None{}
    }
    retv13 = jp15
    return retv13
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv20 Option__int32
    var mtmp0 Option__int32 = maybe_seed(flag__2)
    var jp22 int32
    switch mtmp0.(type) {
    case None:
        retv20 = None{}
        return retv20
    case Some:
        var x1 int32 = mtmp0.(Some)._0
        var try_value__22 int32 = x1
        jp22 = try_value__22
        var a__3 int32 = jp22
        var mtmp2 Option__int32 = maybe_double(a__3)
        var jp24 int32
        switch mtmp2.(type) {
        case None:
            retv20 = None{}
            return retv20
        case Some:
            var x3 int32 = mtmp2.(Some)._0
            var try_value__26 int32 = x3
            jp24 = try_value__26
            var b__4 int32 = jp24
            var t25 int32 = a__3 + b__4
            var t26 Option__int32 = Some{
                _0: t25,
            }
            retv20 = t26
            return retv20
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv28 string
    var jp30 string
    switch opt__5.(type) {
    case None:
        jp30 = "none"
    case Some:
        var x4 int32 = opt__5.(Some)._0
        var value__6 int32 = x4
        var t31 string = int32_to_string(value__6)
        var t32 string = "some=" + t31
        jp30 = t32
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t34 Option__int32 = maybe_total(true)
    var t35 string = show(t34)
    println__T_string(t35)
    var t36 Option__int32 = maybe_total(false)
    var t37 string = show(t36)
    println__T_string(t37)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
