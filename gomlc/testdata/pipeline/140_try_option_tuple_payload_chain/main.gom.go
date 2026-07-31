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
    var retv161 _goml_m_Option_____o_string_c_string_q_
    var jp163 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t164 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t165 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t164,
        }
        jp163 = t165
    } else {
        jp163 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv161 = jp163
    return retv161
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv167 _goml_m_Option_____o_string_c_string_q_
    var t168 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv167 = t168
    return retv167
}

func describe(ok__2 bool) Option__string {
    var retv170 Option__string
    var mtmp152 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp172 Tuple2_6string_6string
    switch mtmp152.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv170 = Option__string_None{}
        return retv170
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x153 Tuple2_6string_6string = mtmp152.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x153
        jp172 = try_value__16
        var mtmp154 Tuple2_6string_6string = jp172
        var x155 string = mtmp154._0
        var x156 string = mtmp154._1
        var after__4 string = x156
        var before__3 string = x155
        var t173 string = before__3 + ":"
        var t174 string = t173 + after__4
        var t175 Option__string = Option__string_Some{
            _0: t174,
        }
        retv170 = t175
        return retv170
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv177 string
    var jp179 string
    switch opt__5.(type) {
    case Option__string_None:
        jp179 = "none"
    case Option__string_Some:
        var x157 string = opt__5.(Option__string_Some)._0
        var value__6 string = x157
        var t180 string = "some " + value__6
        jp179 = t180
    default:
        panic("non-exhaustive match")
    }
    retv177 = jp179
    return retv177
}

func main0() struct{} {
    var t182 Option__string = describe(true)
    var t183 string = show(t182)
    println__T_string(t183)
    var t184 Option__string = describe(false)
    var t185 string = show(t184)
    println__T_string(t185)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv190 string
    retv190 = self__38
    return retv190
}

func main() {
    main0()
}
