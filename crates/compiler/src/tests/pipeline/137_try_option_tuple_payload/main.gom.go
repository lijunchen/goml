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
            _0: "alpha",
            _1: "beta",
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

func describe(ok__1 bool) Option__string {
    var retv19 Option__string
    var mtmp4 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp21 Tuple2_6string_6string
    switch mtmp4.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv19 = Option__string_None{}
        return retv19
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x5 Tuple2_6string_6string = mtmp4.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x5
        jp21 = try_value__13
        var mtmp6 Tuple2_6string_6string = jp21
        var x7 string = mtmp6._0
        var x8 string = mtmp6._1
        var after__3 string = x8
        var before__2 string = x7
        var t22 string = before__2 + "|"
        var t23 string = t22 + after__3
        var t24 Option__string = Option__string_Some{
            _0: t23,
        }
        retv19 = t24
        return retv19
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv26 string
    var jp28 string
    switch opt__4.(type) {
    case Option__string_None:
        jp28 = "none"
    case Option__string_Some:
        var x9 string = opt__4.(Option__string_Some)._0
        var value__5 string = x9
        var t29 string = "some " + value__5
        jp28 = t29
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func main0() struct{} {
    var t31 Option__string = describe(true)
    var t32 string = show(t31)
    println__T_string(t32)
    var t33 Option__string = describe(false)
    var t34 string = show(t33)
    println__T_string(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
