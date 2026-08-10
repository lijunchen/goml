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
        var t184 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t185 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t184,
        }
        return t185
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp172 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline209 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline210 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline209,
        }
        mtmp172 = inline210
    } else {
        mtmp172 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp189 Tuple2_6string_6string
    switch mtmp172.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x173 Tuple2_6string_6string = mtmp172.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp189 = x173
        var x175 string = jp189._0
        var x176 string = jp189._1
        var t190 string = x175 + "|"
        var t191 string = t190 + x176
        var t192 Option__string = Option__string_Some{
            _0: t191,
        }
        return t192
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t199 Option__string = describe(true)
    var t200 string
    switch t199.(type) {
    case Option__string_None:
        t200 = "none"
    case Option__string_Some:
        var inline237 string = t199.(Option__string_Some)._0
        var inline239 string = "some " + inline237
        t200 = inline239
    default:
        panic("non-exhaustive match")
    }
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline234)
    var t201 Option__string
    var inline219 bool = false
    var inline220 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline219)
    var inline222 Tuple2_6string_6string
    switch inline220.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t201 = Option__string_None{}
        var t202 string
        switch t201.(type) {
        case Option__string_None:
            t202 = "none"
        case Option__string_Some:
            var inline215 string = t201.(Option__string_Some)._0
            var inline217 string = "some " + inline215
            t202 = inline217
        default:
            panic("non-exhaustive match")
        }
        var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
        _goml_runtime_core_string_println(inline212)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline231 Tuple2_6string_6string = inline220.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline222 = inline231
        var inline224 string = inline222._0
        var inline225 string = inline222._1
        var inline228 string = inline224 + "|"
        var inline229 string = inline228 + inline225
        var inline230 Option__string = Option__string_Some{
            _0: inline229,
        }
        t201 = inline230
        var t202 string
        switch t201.(type) {
        case Option__string_None:
            t202 = "none"
        case Option__string_Some:
            var inline215 string = t201.(Option__string_Some)._0
            var inline217 string = "some " + inline215
            t202 = inline217
        default:
            panic("non-exhaustive match")
        }
        var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
        _goml_runtime_core_string_println(inline212)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
