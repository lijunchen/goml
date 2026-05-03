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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ {
    var retv15 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_
    var t16 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = cut_pair(ok__1)
    retv15 = t16
    return retv15
}

func describe(ok__2 bool) Option__string {
    var retv18 Option__string
    var mtmp0 _goml_Option_x5f__x5f__x28_string_x2c_string_x29_ = pair(ok__2)
    var jp20 Tuple2_6string_6string
    switch mtmp0.(type) {
    case _goml_Option_x5f__x5f__x28_string_x2c_string_x29__None:
        retv18 = Option__string_None{}
        return retv18
    case _goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some:
        var x1 Tuple2_6string_6string = mtmp0.(_goml_Option_x5f__x5f__x28_string_x2c_string_x29__Some)._0
        var try_value__16 Tuple2_6string_6string = x1
        jp20 = try_value__16
        var mtmp2 Tuple2_6string_6string = jp20
        var x3 string = mtmp2._0
        var x4 string = mtmp2._1
        var after__4 string = x4
        var before__3 string = x3
        var t21 string = before__3 + ":"
        var t22 string = t21 + after__4
        var t23 Option__string = Option__string_Some{
            _0: t22,
        }
        retv18 = t23
        return retv18
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv25 string
    var jp27 string
    switch opt__5.(type) {
    case Option__string_None:
        jp27 = "none"
    case Option__string_Some:
        var x5 string = opt__5.(Option__string_Some)._0
        var value__6 string = x5
        var t28 string = "some " + value__6
        jp27 = t28
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t30 Option__string = describe(true)
    var t31 string = show(t30)
    println__T_string(t31)
    var t32 Option__string = describe(false)
    var t33 string = show(t32)
    println__T_string(t33)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
