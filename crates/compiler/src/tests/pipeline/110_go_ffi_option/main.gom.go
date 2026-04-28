package main

import (
    _goml_fmt "fmt"
    _goml_pkg_strings "strings"
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

type GoError = error

func cut_prefix_ffi_wrap(p0 string, p1 string) Option__string {
    var ffi_value_0 string
    var ffi_ok bool
    ffi_value_0, ffi_ok = _goml_pkg_strings.CutPrefix(p0, p1)
    if ffi_ok {
        return Some{
            _0: ffi_value_0,
        }
    }
    return None{}
}

func describe(input__0 string, prefix__1 string) string {
    var retv5 string
    var mtmp0 Option__string = cut_prefix_ffi_wrap(input__0, prefix__1)
    var jp7 string
    switch mtmp0.(type) {
    case None:
        jp7 = "none"
    case Some:
        var x1 string = mtmp0.(Some)._0
        var rest__2 string = x1
        var t8 string = "some=" + rest__2
        jp7 = t8
    default:
        panic("non-exhaustive match")
    }
    retv5 = jp7
    return retv5
}

func main0() struct{} {
    var t10 string = describe("goml", "go")
    println__T_string(t10)
    var t11 string = describe("goml", "ml")
    println__T_string(t11)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
