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

type _goml_Option___x28_string_x2c_string_x29_ interface {
    is_goml_Option___x28_string_x2c_string_x29_()
}

type None struct {}

func (_ None) is_goml_Option___x28_string_x2c_string_x29_() {}

type Some struct {
    _0 Tuple2_string_string
}

func (_ Some) is_goml_Option___x28_string_x2c_string_x29_() {}

type GoError = error

func pair__native(text__0 string) (string, string, bool) {
    var t8_value_0 string
    var t8_value_1 string
    var t8_ok bool
    t8_value_0, t8_value_1, t8_ok = strings.Cut(text__0, ":")
    if !t8_ok {
        var ret_zero Tuple2_string_string
        return ret_zero._0, ret_zero._1, false
    }
    var ret_payload Tuple2_string_string = Tuple2_string_string{
        _0: t8_value_0,
        _1: t8_value_1,
    }
    return ret_payload._0, ret_payload._1, true
}

func pair(text__0 string) _goml_Option___x28_string_x2c_string_x29_ {
    var native_value_0 string
    var native_value_1 string
    var native_ok bool
    native_value_0, native_value_1, native_ok = pair__native(text__0)
    if native_ok {
        return Some{
            _0: Tuple2_string_string{
                _0: native_value_0,
                _1: native_value_1,
            },
        }
    }
    return None{}
}

func describe(text__1 string) string {
    var retv10 string
    var mtmp0 _goml_Option___x28_string_x2c_string_x29_ = pair(text__1)
    var jp12 string
    switch mtmp0.(type) {
    case None:
        jp12 = "missing"
    case Some:
        var x1 Tuple2_string_string = mtmp0.(Some)._0
        var x2 string = x1._0
        var x3 string = x1._1
        var after__3 string = x3
        var before__2 string = x2
        var t13 string = before__2 + "|"
        var t14 string = t13 + after__3
        jp12 = t14
    default:
        panic("non-exhaustive match")
    }
    retv10 = jp12
    return retv10
}

func main0() struct{} {
    var t16 string = describe("alpha:beta")
    println__T_string(t16)
    var t17 string = describe("plain")
    println__T_string(t17)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
