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

func trim_go__native(input__0 string) (string, bool) {
    var jp8 string
    var mtmp0_value_0 string
    var mtmp0_ok bool
    mtmp0_value_0, mtmp0_ok = strings.CutPrefix(input__0, "go")
    if !mtmp0_ok {
        var ret_zero string
        return ret_zero, false
    }
    jp8 = mtmp0_value_0
    var t9 string = jp8 + "!"
    return t9, true
}

func trim_go(input__0 string) Option__string {
    var native_value_0 string
    var native_ok bool
    native_value_0, native_ok = trim_go__native(input__0)
    if native_ok {
        return Some{
            _0: native_value_0,
        }
    }
    return None{}
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
