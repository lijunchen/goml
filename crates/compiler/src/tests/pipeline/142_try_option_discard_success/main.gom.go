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
    var retv68 _goml_m_Option_____o_string_c_string_q_
    var jp70 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t71 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t72 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t71,
        }
        jp70 = t72
    } else {
        jp70 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv68 = jp70
    return retv68
}

func check(ok__1 bool) Option__string {
    var retv74 Option__string
    var mtmp61 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp61.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv74 = Option__string_None{}
        return retv74
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t77 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv74 = t77
        return retv74
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv79 string
    var jp81 string
    switch opt__2.(type) {
    case Option__string_None:
        jp81 = "none"
    case Option__string_Some:
        var x64 string = opt__2.(Option__string_Some)._0
        var value__3 string = x64
        var t82 string = "some " + value__3
        jp81 = t82
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var t84 Option__string = check(true)
    var t85 string = show(t84)
    println__T_string(t85)
    var t86 Option__string = check(false)
    var t87 string = show(t86)
    println__T_string(t87)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv92 string
    retv92 = self__37
    return retv92
}

func main() {
    main0()
}
