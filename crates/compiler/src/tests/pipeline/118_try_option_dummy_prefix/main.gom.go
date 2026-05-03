package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func cut_prefix(case_id__0 int32) Option__string {
    var retv6 Option__string
    var t9 bool = case_id__0 == 0
    var jp8 Option__string
    if t9 {
        var t10 Option__string = Some{
            _0: "ml",
        }
        jp8 = t10
    } else {
        jp8 = None{}
    }
    retv6 = jp8
    return retv6
}

func trim_go(case_id__1 int32) Option__string {
    var retv12 Option__string
    var mtmp0 Option__string = cut_prefix(case_id__1)
    var jp14 string
    switch mtmp0.(type) {
    case None:
        retv12 = None{}
        return retv12
    case Some:
        var x1 string = mtmp0.(Some)._0
        var try_value__13 string = x1
        jp14 = try_value__13
        var suffix__2 string = jp14
        var t15 string = suffix__2 + "!"
        var t16 Option__string = Some{
            _0: t15,
        }
        retv12 = t16
        return retv12
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv18 string
    var jp20 string
    switch opt__3.(type) {
    case None:
        jp20 = "none"
    case Some:
        var x2 string = opt__3.(Some)._0
        var value__4 string = x2
        var t21 string = "some " + value__4
        jp20 = t21
    default:
        panic("non-exhaustive match")
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var t23 Option__string = trim_go(0)
    var t24 string = show(t23)
    println__T_string(t24)
    var t25 Option__string = trim_go(1)
    var t26 string = show(t25)
    println__T_string(t26)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
