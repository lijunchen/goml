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
        var t167 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t168 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t167,
        }
        return t168
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline195 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline196 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline195,
        }
        return inline196
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp155 _goml_m_Option_____o_string_c_string_q_
    var inline198 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp155 = inline198
    var jp175 Tuple2_6string_6string
    switch mtmp155.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x156 Tuple2_6string_6string = mtmp155.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp175 = x156
        var x158 string = jp175._0
        var x159 string = jp175._1
        var t176 string = x158 + ":"
        var t177 string = t176 + x159
        var t178 Option__string = Option__string_Some{
            _0: t177,
        }
        return t178
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t185 Option__string = describe(true)
    var t186 string
    switch t185.(type) {
    case Option__string_None:
        t186 = "none"
    case Option__string_Some:
        var inline225 string = t185.(Option__string_Some)._0
        var inline227 string = "some " + inline225
        t186 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline222)
    var t187 Option__string
    var inline207 bool = false
    var inline208 _goml_m_Option_____o_string_c_string_q_ = pair(inline207)
    var inline210 Tuple2_6string_6string
    switch inline208.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t187 = Option__string_None{}
        var t188 string
        switch t187.(type) {
        case Option__string_None:
            t188 = "none"
        case Option__string_Some:
            var inline203 string = t187.(Option__string_Some)._0
            var inline205 string = "some " + inline203
            t188 = inline205
        default:
            panic("non-exhaustive match")
        }
        var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
        _goml_runtime_core_string_println(inline200)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline219 Tuple2_6string_6string = inline208.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline210 = inline219
        var inline212 string = inline210._0
        var inline213 string = inline210._1
        var inline216 string = inline212 + ":"
        var inline217 string = inline216 + inline213
        var inline218 Option__string = Option__string_Some{
            _0: inline217,
        }
        t187 = inline218
        var t188 string
        switch t187.(type) {
        case Option__string_None:
            t188 = "none"
        case Option__string_Some:
            var inline203 string = t187.(Option__string_Some)._0
            var inline205 string = "some " + inline203
            t188 = inline205
        default:
            panic("non-exhaustive match")
        }
        var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
        _goml_runtime_core_string_println(inline200)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
