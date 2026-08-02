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
    var retv162 _goml_m_Option_____o_string_c_string_q_
    var jp164 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t165 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t166 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t165,
        }
        jp164 = t166
    } else {
        jp164 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv162 = jp164
    return retv162
}

func check(ok__1 bool) Option__string {
    var retv168 Option__string
    var mtmp155 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp155.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv168 = Option__string_None{}
        return retv168
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t171 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv168 = t171
        return retv168
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv173 string
    var jp175 string
    switch opt__2.(type) {
    case Option__string_None:
        jp175 = "none"
    case Option__string_Some:
        var x158 string = opt__2.(Option__string_Some)._0
        var value__3 string = x158
        var t176 string = "some " + value__3
        jp175 = t176
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
}

func main0() struct{} {
    var t178 Option__string = check(true)
    var t179 string = show(t178)
    println__T_string(t179)
    var t180 Option__string = check(false)
    var t181 string = show(t180)
    println__T_string(t181)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv186 string
    retv186 = self__38
    return retv186
}

func main() {
    main0()
}
