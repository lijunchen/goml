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
        var t199 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t200 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t199,
        }
        return t200
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp187 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline224 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline225 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline224,
        }
        mtmp187 = inline225
    } else {
        mtmp187 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp204 Tuple2_6string_6string
    switch mtmp187.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x188 Tuple2_6string_6string = mtmp187.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp204 = x188
        var x190 string = jp204._0
        var x191 string = jp204._1
        var t205 string = x190 + "|"
        var t206 string = t205 + x191
        var t207 Option__string = Option__string_Some{
            _0: t206,
        }
        return t207
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t214 Option__string = describe(true)
    var t215 string
    switch t214.(type) {
    case Option__string_None:
        t215 = "none"
    case Option__string_Some:
        var inline252 string = t214.(Option__string_Some)._0
        var inline254 string = "some " + inline252
        t215 = inline254
    default:
        panic("non-exhaustive match")
    }
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline249)
    var t216 Option__string
    var inline234 bool = false
    var inline235 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline234)
    var inline237 Tuple2_6string_6string
    switch inline235.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t216 = Option__string_None{}
        var t217 string
        switch t216.(type) {
        case Option__string_None:
            t217 = "none"
        case Option__string_Some:
            var inline230 string = t216.(Option__string_Some)._0
            var inline232 string = "some " + inline230
            t217 = inline232
        default:
            panic("non-exhaustive match")
        }
        var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
        _goml_runtime_core_string_println(inline227)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline246 Tuple2_6string_6string = inline235.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline237 = inline246
        var inline239 string = inline237._0
        var inline240 string = inline237._1
        var inline243 string = inline239 + "|"
        var inline244 string = inline243 + inline240
        var inline245 Option__string = Option__string_Some{
            _0: inline244,
        }
        t216 = inline245
        var t217 string
        switch t216.(type) {
        case Option__string_None:
            t217 = "none"
        case Option__string_Some:
            var inline230 string = t216.(Option__string_Some)._0
            var inline232 string = "some " + inline230
            t217 = inline232
        default:
            panic("non-exhaustive match")
        }
        var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
        _goml_runtime_core_string_println(inline227)
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
