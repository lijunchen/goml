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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv79 _goml_m_Option_____o_string_c_string_q_
    var t80 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv79 = t80
    return retv79
}

func describe(ok__2 bool) Option__string {
    var retv82 Option__string
    var mtmp64 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp84 Tuple2_6string_6string
    switch mtmp64.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv82 = Option__string_None{}
        return retv82
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x65 Tuple2_6string_6string = mtmp64.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x65
        jp84 = try_value__16
        var mtmp66 Tuple2_6string_6string = jp84
        var x67 string = mtmp66._0
        var x68 string = mtmp66._1
        var after__4 string = x68
        var before__3 string = x67
        var t85 string = before__3 + ":"
        var t86 string = t85 + after__4
        var t87 Option__string = Option__string_Some{
            _0: t86,
        }
        retv82 = t87
        return retv82
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv89 string
    var jp91 string
    switch opt__5.(type) {
    case Option__string_None:
        jp91 = "none"
    case Option__string_Some:
        var x69 string = opt__5.(Option__string_Some)._0
        var value__6 string = x69
        var t92 string = "some " + value__6
        jp91 = t92
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
}

func main0() struct{} {
    var t94 Option__string = describe(true)
    var t95 string = show(t94)
    println__T_string(t95)
    var t96 Option__string = describe(false)
    var t97 string = show(t96)
    println__T_string(t97)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv102 string
    retv102 = self__38
    return retv102
}

func main() {
    main0()
}
