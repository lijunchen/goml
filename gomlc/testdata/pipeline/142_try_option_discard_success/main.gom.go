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
    var retv75 _goml_m_Option_____o_string_c_string_q_
    var jp77 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t78 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t79 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t78,
        }
        jp77 = t79
    } else {
        jp77 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv75 = jp77
    return retv75
}

func check(ok__1 bool) Option__string {
    var retv81 Option__string
    var mtmp68 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp68.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv81 = Option__string_None{}
        return retv81
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t84 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv81 = t84
        return retv81
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv86 string
    var jp88 string
    switch opt__2.(type) {
    case Option__string_None:
        jp88 = "none"
    case Option__string_Some:
        var x71 string = opt__2.(Option__string_Some)._0
        var value__3 string = x71
        var t89 string = "some " + value__3
        jp88 = t89
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t91 Option__string = check(true)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Option__string = check(false)
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
