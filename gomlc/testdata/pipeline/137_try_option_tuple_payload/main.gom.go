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
        var t189 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t190 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t189,
        }
        return t190
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp177 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline214 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline215 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline214,
        }
        mtmp177 = inline215
    } else {
        mtmp177 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp194 Tuple2_6string_6string
    switch mtmp177.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x178 Tuple2_6string_6string = mtmp177.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp194 = x178
        var x180 string = jp194._0
        var x181 string = jp194._1
        var t195 string = x180 + "|"
        var t196 string = t195 + x181
        var t197 Option__string = Option__string_Some{
            _0: t196,
        }
        return t197
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t204 Option__string = describe(true)
    var t205 string
    switch t204.(type) {
    case Option__string_None:
        t205 = "none"
    case Option__string_Some:
        var inline242 string = t204.(Option__string_Some)._0
        var inline244 string = "some " + inline242
        t205 = inline244
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline239)
    var t206 Option__string
    var inline224 bool = false
    var inline225 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline224)
    var inline227 Tuple2_6string_6string
    switch inline225.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t206 = Option__string_None{}
        var t207 string
        switch t206.(type) {
        case Option__string_None:
            t207 = "none"
        case Option__string_Some:
            var inline220 string = t206.(Option__string_Some)._0
            var inline222 string = "some " + inline220
            t207 = inline222
        default:
            panic("non-exhaustive match")
        }
        var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
        _goml_runtime_core_string_println(inline217)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline236 Tuple2_6string_6string = inline225.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline227 = inline236
        var inline229 string = inline227._0
        var inline230 string = inline227._1
        var inline233 string = inline229 + "|"
        var inline234 string = inline233 + inline230
        var inline235 Option__string = Option__string_Some{
            _0: inline234,
        }
        t206 = inline235
        var t207 string
        switch t206.(type) {
        case Option__string_None:
            t207 = "none"
        case Option__string_Some:
            var inline220 string = t206.(Option__string_Some)._0
            var inline222 string = "some " + inline220
            t207 = inline222
        default:
            panic("non-exhaustive match")
        }
        var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
        _goml_runtime_core_string_println(inline217)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
