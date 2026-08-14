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
            _0: "left",
            _1: "right",
        }
        var t200 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t199,
        }
        return t200
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline227 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline228 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline227,
        }
        return inline228
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp187 _goml_m_Option_____o_string_c_string_q_
    var inline230 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp187 = inline230
    var jp207 Tuple2_6string_6string
    switch mtmp187.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x188 Tuple2_6string_6string = mtmp187.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp207 = x188
        var x190 string = jp207._0
        var x191 string = jp207._1
        var t208 string = x190 + ":"
        var t209 string = t208 + x191
        var t210 Option__string = Option__string_Some{
            _0: t209,
        }
        return t210
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t217 Option__string = describe(true)
    var t218 string
    switch t217.(type) {
    case Option__string_None:
        t218 = "none"
    case Option__string_Some:
        var inline257 string = t217.(Option__string_Some)._0
        var inline259 string = "some " + inline257
        t218 = inline259
    default:
        panic("non-exhaustive match")
    }
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline254)
    var t219 Option__string
    var inline239 bool = false
    var inline240 _goml_m_Option_____o_string_c_string_q_ = pair(inline239)
    var inline242 Tuple2_6string_6string
    switch inline240.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t219 = Option__string_None{}
        var t220 string
        switch t219.(type) {
        case Option__string_None:
            t220 = "none"
        case Option__string_Some:
            var inline235 string = t219.(Option__string_Some)._0
            var inline237 string = "some " + inline235
            t220 = inline237
        default:
            panic("non-exhaustive match")
        }
        var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
        _goml_runtime_core_string_println(inline232)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline251 Tuple2_6string_6string = inline240.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline242 = inline251
        var inline244 string = inline242._0
        var inline245 string = inline242._1
        var inline248 string = inline244 + ":"
        var inline249 string = inline248 + inline245
        var inline250 Option__string = Option__string_Some{
            _0: inline249,
        }
        t219 = inline250
        var t220 string
        switch t219.(type) {
        case Option__string_None:
            t220 = "none"
        case Option__string_Some:
            var inline235 string = t219.(Option__string_Some)._0
            var inline237 string = "some " + inline235
            t220 = inline237
        default:
            panic("non-exhaustive match")
        }
        var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
        _goml_runtime_core_string_println(inline232)
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
