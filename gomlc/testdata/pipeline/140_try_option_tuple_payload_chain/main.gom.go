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
    var retv77 _goml_m_Option_____o_string_c_string_q_
    var jp79 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t80 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t81 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t80,
        }
        jp79 = t81
    } else {
        jp79 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv77 = jp79
    return retv77
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv83 _goml_m_Option_____o_string_c_string_q_
    var t84 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv83 = t84
    return retv83
}

func describe(ok__2 bool) Option__string {
    var retv86 Option__string
    var mtmp68 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp88 Tuple2_6string_6string
    switch mtmp68.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv86 = Option__string_None{}
        return retv86
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x69 Tuple2_6string_6string = mtmp68.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x69
        jp88 = try_value__16
        var mtmp70 Tuple2_6string_6string = jp88
        var x71 string = mtmp70._0
        var x72 string = mtmp70._1
        var after__4 string = x72
        var before__3 string = x71
        var t89 string = before__3 + ":"
        var t90 string = t89 + after__4
        var t91 Option__string = Option__string_Some{
            _0: t90,
        }
        retv86 = t91
        return retv86
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv93 string
    var jp95 string
    switch opt__5.(type) {
    case Option__string_None:
        jp95 = "none"
    case Option__string_Some:
        var x73 string = opt__5.(Option__string_Some)._0
        var value__6 string = x73
        var t96 string = "some " + value__6
        jp95 = t96
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t98 Option__string = describe(true)
    var t99 string = show(t98)
    println__T_string(t99)
    var t100 Option__string = describe(false)
    var t101 string = show(t100)
    println__T_string(t101)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t103 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv106 string
    retv106 = self__38
    return retv106
}

func main() {
    main0()
}
