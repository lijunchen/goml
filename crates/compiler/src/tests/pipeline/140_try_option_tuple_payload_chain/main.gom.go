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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv22 _goml_m_Option_____o_string_c_string_q_
    var t23 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv22 = t23
    return retv22
}

func describe(ok__2 bool) Option__string {
    var retv25 Option__string
    var mtmp7 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp27 Tuple2_6string_6string
    switch mtmp7.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv25 = Option__string_None{}
        return retv25
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x8 Tuple2_6string_6string = mtmp7.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x8
        jp27 = try_value__16
        var mtmp9 Tuple2_6string_6string = jp27
        var x10 string = mtmp9._0
        var x11 string = mtmp9._1
        var after__4 string = x11
        var before__3 string = x10
        var t28 string = before__3 + ":"
        var t29 string = t28 + after__4
        var t30 Option__string = Option__string_Some{
            _0: t29,
        }
        retv25 = t30
        return retv25
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv32 string
    var jp34 string
    switch opt__5.(type) {
    case Option__string_None:
        jp34 = "none"
    case Option__string_Some:
        var x12 string = opt__5.(Option__string_Some)._0
        var value__6 string = x12
        var t35 string = "some " + value__6
        jp34 = t35
    default:
        panic("non-exhaustive match")
    }
    retv32 = jp34
    return retv32
}

func main0() struct{} {
    var t37 Option__string = describe(true)
    var t38 string = show(t37)
    println__T_string(t38)
    var t39 Option__string = describe(false)
    var t40 string = show(t39)
    println__T_string(t40)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t42 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t42)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv45 string
    retv45 = self__9
    return retv45
}

func main() {
    main0()
}
