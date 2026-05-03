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
    var retv9 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_
    var jp11 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_
    if ok__0 {
        var t12 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t13 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some{
            _0: t12,
        }
        jp11 = t13
    } else {
        jp11 = _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None{}
    }
    retv9 = jp11
    return retv9
}

func describe(ok__1 bool) Option__string {
    var retv15 Option__string
    var mtmp0 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = cut_pair(ok__1)
    var jp17 Tuple2_6string_6string
    switch mtmp0.(type) {
    case _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None:
        retv15 = Option__string_None{}
        return retv15
    case _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some:
        var x1 Tuple2_6string_6string = mtmp0.(_goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some)._0
        var try_value__13 Tuple2_6string_6string = x1
        jp17 = try_value__13
        var mtmp2 Tuple2_6string_6string = jp17
        var x3 string = mtmp2._0
        var x4 string = mtmp2._1
        var after__3 string = x4
        var before__2 string = x3
        var t18 string = before__2 + "|"
        var t19 string = t18 + after__3
        var t20 Option__string = Option__string_Some{
            _0: t19,
        }
        retv15 = t20
        return retv15
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv22 string
    var jp24 string
    switch opt__4.(type) {
    case Option__string_None:
        jp24 = "none"
    case Option__string_Some:
        var x5 string = opt__4.(Option__string_Some)._0
        var value__5 string = x5
        var t25 string = "some " + value__5
        jp24 = t25
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func main0() struct{} {
    var t27 Option__string = describe(true)
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Option__string = describe(false)
    var t30 string = show(t29)
    println__T_string(t30)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
