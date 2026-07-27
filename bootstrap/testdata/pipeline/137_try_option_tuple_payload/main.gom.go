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
    var retv73 _goml_m_Option_____o_string_c_string_q_
    var jp75 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t76 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t77 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t76,
        }
        jp75 = t77
    } else {
        jp75 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv73 = jp75
    return retv73
}

func describe(ok__1 bool) Option__string {
    var retv79 Option__string
    var mtmp64 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp81 Tuple2_6string_6string
    switch mtmp64.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv79 = Option__string_None{}
        return retv79
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x65 Tuple2_6string_6string = mtmp64.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x65
        jp81 = try_value__13
        var mtmp66 Tuple2_6string_6string = jp81
        var x67 string = mtmp66._0
        var x68 string = mtmp66._1
        var after__3 string = x68
        var before__2 string = x67
        var t82 string = before__2 + "|"
        var t83 string = t82 + after__3
        var t84 Option__string = Option__string_Some{
            _0: t83,
        }
        retv79 = t84
        return retv79
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv86 string
    var jp88 string
    switch opt__4.(type) {
    case Option__string_None:
        jp88 = "none"
    case Option__string_Some:
        var x69 string = opt__4.(Option__string_Some)._0
        var value__5 string = x69
        var t89 string = "some " + value__5
        jp88 = t89
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t91 Option__string = describe(true)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Option__string = describe(false)
    var t94 string = show(t93)
    println__T_string(t94)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv99 string
    retv99 = self__38
    return retv99
}

func main() {
    main0()
}
