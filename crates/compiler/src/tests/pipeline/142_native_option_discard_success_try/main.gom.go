package main

import (
    "fmt"
    "strings"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_string_string struct {
    _0 string
    _1 string
}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type _goml_Option___x28_string_x2c_string_x29_ interface {
    is_goml_Option___x28_string_x2c_string_x29_()
}

type _goml_Option___x28_string_x2c_string_x29__None struct {}

func (_ _goml_Option___x28_string_x2c_string_x29__None) is_goml_Option___x28_string_x2c_string_x29_() {}

type _goml_Option___x28_string_x2c_string_x29__Some struct {
    _0 Tuple2_string_string
}

func (_ _goml_Option___x28_string_x2c_string_x29__Some) is_goml_Option___x28_string_x2c_string_x29_() {}

type GoError = error

func touch__native(text__0 string) (string, bool) {
    var mtmp0_ok bool
    _, _, mtmp0_ok = strings.Cut(text__0, ":")
    if !mtmp0_ok {
        var ret_zero string
        return ret_zero, false
    }
    return "ok", true
}

func touch(text__0 string) Option__string {
    var native_value_0 string
    var native_ok bool
    native_value_0, native_ok = touch__native(text__0)
    if native_ok {
        return Option__string_Some{
            _0: native_value_0,
        }
    }
    return Option__string_None{}
}

func show(value__1 Option__string) string {
    var retv12 string
    var jp14 string
    switch value__1.(type) {
    case Option__string_None:
        jp14 = "none"
    case Option__string_Some:
        var x3 string = value__1.(Option__string_Some)._0
        var text__2 string = x3
        jp14 = text__2
    default:
        panic("non-exhaustive match")
    }
    retv12 = jp14
    return retv12
}

func main0() struct{} {
    var t16 Option__string = touch("left:right")
    var t17 string = show(t16)
    println__T_string(t17)
    var t18 Option__string = touch("plain")
    var t19 string = show(t18)
    println__T_string(t19)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
