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
            _0: "alpha",
            _1: "beta",
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

func describe(ok__1 bool) Option__string {
    var retv83 Option__string
    var mtmp68 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp85 Tuple2_6string_6string
    switch mtmp68.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv83 = Option__string_None{}
        return retv83
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x69 Tuple2_6string_6string = mtmp68.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x69
        jp85 = try_value__13
        var mtmp70 Tuple2_6string_6string = jp85
        var x71 string = mtmp70._0
        var x72 string = mtmp70._1
        var after__3 string = x72
        var before__2 string = x71
        var t86 string = before__2 + "|"
        var t87 string = t86 + after__3
        var t88 Option__string = Option__string_Some{
            _0: t87,
        }
        retv83 = t88
        return retv83
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv90 string
    var jp92 string
    switch opt__4.(type) {
    case Option__string_None:
        jp92 = "none"
    case Option__string_Some:
        var x73 string = opt__4.(Option__string_Some)._0
        var value__5 string = x73
        var t93 string = "some " + value__5
        jp92 = t93
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func main0() struct{} {
    var t95 Option__string = describe(true)
    var t96 string = show(t95)
    println__T_string(t96)
    var t97 Option__string = describe(false)
    var t98 string = show(t97)
    println__T_string(t98)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv103 string
    retv103 = self__38
    return retv103
}

func main() {
    main0()
}
