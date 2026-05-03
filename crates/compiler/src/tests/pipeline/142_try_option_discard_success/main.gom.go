package main

import (
    _goml_fmt "fmt"
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

type _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None struct {}

func (_ _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None) is_goml_Option_x5f__x5f__x28_string_x2c_string_x29_() {}

type _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some) is_goml_Option_x5f__x5f__x28_string_x2c_string_x29_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func cut_pair(ok__0 bool) _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ {
    var retv7 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_
    var jp9 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_
    if ok__0 {
        var t10 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t11 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some{
            _0: t10,
        }
        jp9 = t11
    } else {
        jp9 = _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None{}
    }
    retv7 = jp9
    return retv7
}

func check(ok__1 bool) Option__string {
    var retv13 Option__string
    var mtmp0 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = cut_pair(ok__1)
    switch mtmp0.(type) {
    case _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None:
        retv13 = Option__string_None{}
        return retv13
    case _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some:
        var t16 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv13 = t16
        return retv13
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv18 string
    var jp20 string
    switch opt__2.(type) {
    case Option__string_None:
        jp20 = "none"
    case Option__string_Some:
        var x3 string = opt__2.(Option__string_Some)._0
        var value__3 string = x3
        var t21 string = "some " + value__3
        jp20 = t21
    default:
        panic("non-exhaustive match")
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var t23 Option__string = check(true)
    var t24 string = show(t23)
    println__T_string(t24)
    var t25 Option__string = check(false)
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
