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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv123 _goml_m_Option_____o_string_c_string_q_
    var t124 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv123 = t124
    return retv123
}

func describe(ok__2 bool) Option__string {
    var retv126 Option__string
    var mtmp108 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp128 Tuple2_6string_6string
    switch mtmp108.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv126 = Option__string_None{}
        return retv126
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x109 Tuple2_6string_6string = mtmp108.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x109
        jp128 = try_value__16
        var mtmp110 Tuple2_6string_6string = jp128
        var x111 string = mtmp110._0
        var x112 string = mtmp110._1
        var after__4 string = x112
        var before__3 string = x111
        var t129 string = before__3 + ":"
        var t130 string = t129 + after__4
        var t131 Option__string = Option__string_Some{
            _0: t130,
        }
        retv126 = t131
        return retv126
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv133 string
    var jp135 string
    switch opt__5.(type) {
    case Option__string_None:
        jp135 = "none"
    case Option__string_Some:
        var x113 string = opt__5.(Option__string_Some)._0
        var value__6 string = x113
        var t136 string = "some " + value__6
        jp135 = t136
    default:
        panic("non-exhaustive match")
    }
    retv133 = jp135
    return retv133
}

func main0() struct{} {
    var t138 Option__string = describe(true)
    var t139 string = show(t138)
    println__T_string(t139)
    var t140 Option__string = describe(false)
    var t141 string = show(t140)
    println__T_string(t141)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv146 string
    retv146 = self__38
    return retv146
}

func main() {
    main0()
}
