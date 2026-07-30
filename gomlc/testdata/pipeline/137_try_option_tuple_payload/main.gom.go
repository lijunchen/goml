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
    var retv117 _goml_m_Option_____o_string_c_string_q_
    var jp119 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t120 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t121 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t120,
        }
        jp119 = t121
    } else {
        jp119 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv117 = jp119
    return retv117
}

func describe(ok__1 bool) Option__string {
    var retv123 Option__string
    var mtmp108 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp125 Tuple2_6string_6string
    switch mtmp108.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv123 = Option__string_None{}
        return retv123
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x109 Tuple2_6string_6string = mtmp108.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x109
        jp125 = try_value__13
        var mtmp110 Tuple2_6string_6string = jp125
        var x111 string = mtmp110._0
        var x112 string = mtmp110._1
        var after__3 string = x112
        var before__2 string = x111
        var t126 string = before__2 + "|"
        var t127 string = t126 + after__3
        var t128 Option__string = Option__string_Some{
            _0: t127,
        }
        retv123 = t128
        return retv123
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv130 string
    var jp132 string
    switch opt__4.(type) {
    case Option__string_None:
        jp132 = "none"
    case Option__string_Some:
        var x113 string = opt__4.(Option__string_Some)._0
        var value__5 string = x113
        var t133 string = "some " + value__5
        jp132 = t133
    default:
        panic("non-exhaustive match")
    }
    retv130 = jp132
    return retv130
}

func main0() struct{} {
    var t135 Option__string = describe(true)
    var t136 string = show(t135)
    println__T_string(t136)
    var t137 Option__string = describe(false)
    var t138 string = show(t137)
    println__T_string(t138)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv143 string
    retv143 = self__38
    return retv143
}

func main() {
    main0()
}
