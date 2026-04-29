package main

import (
    _goml_fmt "fmt"
    "strings"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple3_6string_6string_4bool struct {
    _0 string
    _1 string
    _2 bool
}

type GoError = error

func cut_ffi_wrap(p0 string, p1 string) Tuple3_6string_6string_4bool {
    var ffi_value_0 string
    var ffi_value_1 string
    var ffi_value_2 bool
    ffi_value_0, ffi_value_1, ffi_value_2 = strings.Cut(p0, p1)
    return Tuple3_6string_6string_4bool{
        _0: ffi_value_0,
        _1: ffi_value_1,
        _2: ffi_value_2,
    }
}

func describe(input__0 string) string {
    var retv7 string
    var mtmp0 Tuple3_6string_6string_4bool = cut_ffi_wrap(input__0, ":")
    var x1 string = mtmp0._0
    var x2 string = mtmp0._1
    var x3 bool = mtmp0._2
    var found__3 bool = x3
    var after__2 string = x2
    var before__1 string = x1
    var jp9 string
    if found__3 {
        var t10 string = before__1 + "|"
        var t11 string = t10 + after__2
        jp9 = t11
    } else {
        jp9 = "missing"
    }
    retv7 = jp9
    return retv7
}

func main0() struct{} {
    var t13 string = describe("left:right")
    println__T_string(t13)
    var t14 string = describe("plain")
    println__T_string(t14)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
