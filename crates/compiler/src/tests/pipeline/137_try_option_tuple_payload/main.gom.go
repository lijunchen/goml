package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv16 _goml_m_Option_____o_string_c_string_q_
    var jp18 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t19 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t20 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t19,
        }
        jp18 = t20
    } else {
        jp18 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv16 = jp18
    return retv16
}

func describe(ok__1 bool) Option__string {
    var retv22 Option__string
    var mtmp7 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp24 Tuple2_6string_6string
    switch mtmp7.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv22 = Option__string_None{}
        return retv22
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x8 Tuple2_6string_6string = mtmp7.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x8
        jp24 = try_value__13
        var mtmp9 Tuple2_6string_6string = jp24
        var x10 string = mtmp9._0
        var x11 string = mtmp9._1
        var after__3 string = x11
        var before__2 string = x10
        var t25 string = before__2 + "|"
        var t26 string = t25 + after__3
        var t27 Option__string = Option__string_Some{
            _0: t26,
        }
        retv22 = t27
        return retv22
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv29 string
    var jp31 string
    switch opt__4.(type) {
    case Option__string_None:
        jp31 = "none"
    case Option__string_Some:
        var x12 string = opt__4.(Option__string_Some)._0
        var value__5 string = x12
        var t32 string = "some " + value__5
        jp31 = t32
    default:
        panic("non-exhaustive match")
    }
    retv29 = jp31
    return retv29
}

func main0() struct{} {
    var t34 Option__string = describe(true)
    var t35 string = show(t34)
    println__T_string(t35)
    var t36 Option__string = describe(false)
    var t37 string = show(t36)
    println__T_string(t37)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t39 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t39)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
