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
    var retv13 _goml_m_Option_____o_string_c_string_q_
    var jp15 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t16 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t17 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t16,
        }
        jp15 = t17
    } else {
        jp15 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv13 = jp15
    return retv13
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv19 _goml_m_Option_____o_string_c_string_q_
    var t20 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv19 = t20
    return retv19
}

func describe(ok__2 bool) Option__string {
    var retv22 Option__string
    var mtmp4 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp24 Tuple2_6string_6string
    switch mtmp4.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv22 = Option__string_None{}
        return retv22
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x5 Tuple2_6string_6string = mtmp4.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x5
        jp24 = try_value__16
        var mtmp6 Tuple2_6string_6string = jp24
        var x7 string = mtmp6._0
        var x8 string = mtmp6._1
        var after__4 string = x8
        var before__3 string = x7
        var t25 string = before__3 + ":"
        var t26 string = t25 + after__4
        var t27 Option__string = Option__string_Some{
            _0: t26,
        }
        retv22 = t27
        return retv22
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv29 string
    var jp31 string
    switch opt__5.(type) {
    case Option__string_None:
        jp31 = "none"
    case Option__string_Some:
        var x9 string = opt__5.(Option__string_Some)._0
        var value__6 string = x9
        var t32 string = "some " + value__6
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
