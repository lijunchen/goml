package main

import (
    _goml_fmt "fmt"
    "strings"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ interface {
    is_goml_Option_x5f__x5f__x28_string_x2c_string_x29_()
}

type None struct {}

func (_ None) is_goml_Option_x5f__x5f__x28_string_x2c_string_x29_() {}

type Some struct {
    _0 Tuple2_6string_6string
}

func (_ Some) is_goml_Option_x5f__x5f__x28_string_x2c_string_x29_() {}

type GoError = error

func cut_pair_ffi_wrap(p0 string, p1 string) _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ {
    var ffi_value_0 string
    var ffi_value_1 string
    var ffi_ok bool
    ffi_value_0, ffi_value_1, ffi_ok = strings.Cut(p0, p1)
    if ffi_ok {
        return Some{
            _0: Tuple2_6string_6string{
                _0: ffi_value_0,
                _1: ffi_value_1,
            },
        }
    }
    return None{}
}

func describe(text__0 string) string {
    var retv7 string
    var mtmp0 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = cut_pair_ffi_wrap(text__0, ":")
    var jp9 string
    switch mtmp0.(type) {
    case None:
        jp9 = "missing"
    case Some:
        var x1 Tuple2_6string_6string = mtmp0.(Some)._0
        var x2 string = x1._0
        var x3 string = x1._1
        var after__2 string = x3
        var before__1 string = x2
        var t10 string = before__1 + "|"
        var t11 string = t10 + after__2
        jp9 = t11
    default:
        panic("non-exhaustive match")
    }
    retv7 = jp9
    return retv7
}

func main0() struct{} {
    var t13 string = describe("alpha:beta")
    string_println(t13)
    var t14 string = describe("plain")
    string_println(t14)
    return struct{}{}
}

func main() {
    main0()
}
