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
    var retv164 _goml_m_Option_____o_string_c_string_q_
    var jp166 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t167 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t168 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t167,
        }
        jp166 = t168
    } else {
        jp166 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv164 = jp166
    return retv164
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv170 _goml_m_Option_____o_string_c_string_q_
    var t171 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv170 = t171
    return retv170
}

func describe(ok__2 bool) Option__string {
    var retv173 Option__string
    var mtmp155 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp175 Tuple2_6string_6string
    switch mtmp155.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv173 = Option__string_None{}
        return retv173
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x156 Tuple2_6string_6string = mtmp155.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x156
        jp175 = try_value__16
        var mtmp157 Tuple2_6string_6string = jp175
        var x158 string = mtmp157._0
        var x159 string = mtmp157._1
        var after__4 string = x159
        var before__3 string = x158
        var t176 string = before__3 + ":"
        var t177 string = t176 + after__4
        var t178 Option__string = Option__string_Some{
            _0: t177,
        }
        retv173 = t178
        return retv173
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv180 string
    var jp182 string
    switch opt__5.(type) {
    case Option__string_None:
        jp182 = "none"
    case Option__string_Some:
        var x160 string = opt__5.(Option__string_Some)._0
        var value__6 string = x160
        var t183 string = "some " + value__6
        jp182 = t183
    default:
        panic("non-exhaustive match")
    }
    retv180 = jp182
    return retv180
}

func main0() struct{} {
    var t185 Option__string = describe(true)
    var t186 string = show(t185)
    println__T_string(t186)
    var t187 Option__string = describe(false)
    var t188 string = show(t187)
    println__T_string(t188)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv193 string
    retv193 = self__38
    return retv193
}

func main() {
    main0()
}
