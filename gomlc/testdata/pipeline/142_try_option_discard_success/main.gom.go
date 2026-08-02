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
    if ok__0 {
        var t165 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t166 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t165,
        }
        return t166
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func check(ok__1 bool) Option__string {
    var mtmp155 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp155.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t171 Option__string = Option__string_Some{
            _0: "ok",
        }
        return t171
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    switch opt__2.(type) {
    case Option__string_None:
        return "none"
    case Option__string_Some:
        var x158 string = opt__2.(Option__string_Some)._0
        var t176 string = "some " + x158
        return t176
    default:
        panic("non-exhaustive match")
    }
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
    return self__38
}

func main() {
    main0()
}
