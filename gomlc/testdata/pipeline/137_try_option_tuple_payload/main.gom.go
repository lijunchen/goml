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
            _0: "alpha",
            _1: "beta",
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

func describe(ok__1 bool) Option__string {
    var retv167 Option__string
    var mtmp152 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp169 Tuple2_6string_6string
    switch mtmp152.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv167 = Option__string_None{}
        return retv167
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x153 Tuple2_6string_6string = mtmp152.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x153
        jp169 = try_value__13
        var mtmp154 Tuple2_6string_6string = jp169
        var x155 string = mtmp154._0
        var x156 string = mtmp154._1
        var after__3 string = x156
        var before__2 string = x155
        var t170 string = before__2 + "|"
        var t171 string = t170 + after__3
        var t172 Option__string = Option__string_Some{
            _0: t171,
        }
        retv167 = t172
        return retv167
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv174 string
    var jp176 string
    switch opt__4.(type) {
    case Option__string_None:
        jp176 = "none"
    case Option__string_Some:
        var x157 string = opt__4.(Option__string_Some)._0
        var value__5 string = x157
        var t177 string = "some " + value__5
        jp176 = t177
    default:
        panic("non-exhaustive match")
    }
    retv174 = jp176
    return retv174
}

func main0() struct{} {
    var t179 Option__string = describe(true)
    var t180 string = show(t179)
    println__T_string(t180)
    var t181 Option__string = describe(false)
    var t182 string = show(t181)
    println__T_string(t182)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv187 string
    retv187 = self__38
    return retv187
}

func main() {
    main0()
}
