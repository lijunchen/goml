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
    var retv115 _goml_m_Option_____o_string_c_string_q_
    var jp117 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t118 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t119 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t118,
        }
        jp117 = t119
    } else {
        jp117 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv115 = jp117
    return retv115
}

func check(ok__1 bool) Option__string {
    var retv121 Option__string
    var mtmp108 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp108.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv121 = Option__string_None{}
        return retv121
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t124 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv121 = t124
        return retv121
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv126 string
    var jp128 string
    switch opt__2.(type) {
    case Option__string_None:
        jp128 = "none"
    case Option__string_Some:
        var x111 string = opt__2.(Option__string_Some)._0
        var value__3 string = x111
        var t129 string = "some " + value__3
        jp128 = t129
    default:
        panic("non-exhaustive match")
    }
    retv126 = jp128
    return retv126
}

func main0() struct{} {
    var t131 Option__string = check(true)
    var t132 string = show(t131)
    println__T_string(t132)
    var t133 Option__string = check(false)
    var t134 string = show(t133)
    println__T_string(t134)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t136 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t136)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv139 string
    retv139 = self__38
    return retv139
}

func main() {
    main0()
}
