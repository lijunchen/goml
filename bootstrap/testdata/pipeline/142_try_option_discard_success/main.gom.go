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
    var retv71 _goml_m_Option_____o_string_c_string_q_
    var jp73 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t74 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t75 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t74,
        }
        jp73 = t75
    } else {
        jp73 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv71 = jp73
    return retv71
}

func check(ok__1 bool) Option__string {
    var retv77 Option__string
    var mtmp64 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp64.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv77 = Option__string_None{}
        return retv77
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t80 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv77 = t80
        return retv77
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv82 string
    var jp84 string
    switch opt__2.(type) {
    case Option__string_None:
        jp84 = "none"
    case Option__string_Some:
        var x67 string = opt__2.(Option__string_Some)._0
        var value__3 string = x67
        var t85 string = "some " + value__3
        jp84 = t85
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var t87 Option__string = check(true)
    var t88 string = show(t87)
    println__T_string(t88)
    var t89 Option__string = check(false)
    var t90 string = show(t89)
    println__T_string(t90)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv95 string
    retv95 = self__38
    return retv95
}

func main() {
    main0()
}
