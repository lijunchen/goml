package main

import (
    "fmt"
    "strings"
)

func string_println(s string) struct{} {
    fmt.Println(s)
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

type GoError = error

func cut_prefix_ffi_wrap(p0 string, p1 string) Option__string {
    var ffi_value string
    var ffi_ok bool
    ffi_value, ffi_ok = strings.CutPrefix(p0, p1)
    if ffi_ok {
        return Some{
            _0: ffi_value,
        }
    }
    return None{}
}

func trim_go(input__0 string) Option__string {
    var retv6 Option__string
    var mtmp0 Option__string = cut_prefix_ffi_wrap(input__0, "go")
    var jp8 string
    switch mtmp0.(type) {
    case None:
        retv6 = None{}
        return retv6
    case Some:
        var x1 string = mtmp0.(Some)._0
        var try_value__5 string = x1
        jp8 = try_value__5
        var t9 string = jp8 + "!"
        var t10 Option__string = Some{
            _0: t9,
        }
        retv6 = t10
        return retv6
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__1 Option__string) string {
    var retv12 string
    var jp14 string
    switch opt__1.(type) {
    case None:
        jp14 = "none"
    case Some:
        var x2 string = opt__1.(Some)._0
        var value__2 string = x2
        var t15 string = "some=" + value__2
        jp14 = t15
    default:
        panic("non-exhaustive match")
    }
    retv12 = jp14
    return retv12
}

func main0() struct{} {
    var t17 Option__string = trim_go("goml")
    var t18 string = show(t17)
    println__T_string(t18)
    var t19 Option__string = trim_go("ml")
    var t20 string = show(t19)
    println__T_string(t20)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
