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
            _0: "left",
            _1: "right",
        }
        var t190 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t189,
        }
        return t190
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline217 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline218 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline217,
        }
        return inline218
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp177 _goml_m_Option_____o_string_c_string_q_
    var inline220 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp177 = inline220
    var jp197 Tuple2_6string_6string
    switch mtmp177.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x178 Tuple2_6string_6string = mtmp177.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp197 = x178
        var x180 string = jp197._0
        var x181 string = jp197._1
        var t198 string = x180 + ":"
        var t199 string = t198 + x181
        var t200 Option__string = Option__string_Some{
            _0: t199,
        }
        return t200
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t207 Option__string = describe(true)
    var t208 string
    switch t207.(type) {
    case Option__string_None:
        t208 = "none"
    case Option__string_Some:
        var inline247 string = t207.(Option__string_Some)._0
        var inline249 string = "some " + inline247
        t208 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline244)
    var t209 Option__string
    var inline229 bool = false
    var inline230 _goml_m_Option_____o_string_c_string_q_ = pair(inline229)
    var inline232 Tuple2_6string_6string
    switch inline230.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t209 = Option__string_None{}
        var t210 string
        switch t209.(type) {
        case Option__string_None:
            t210 = "none"
        case Option__string_Some:
            var inline225 string = t209.(Option__string_Some)._0
            var inline227 string = "some " + inline225
            t210 = inline227
        default:
            panic("non-exhaustive match")
        }
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline222)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline241 Tuple2_6string_6string = inline230.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline232 = inline241
        var inline234 string = inline232._0
        var inline235 string = inline232._1
        var inline238 string = inline234 + ":"
        var inline239 string = inline238 + inline235
        var inline240 Option__string = Option__string_Some{
            _0: inline239,
        }
        t209 = inline240
        var t210 string
        switch t209.(type) {
        case Option__string_None:
            t210 = "none"
        case Option__string_Some:
            var inline225 string = t209.(Option__string_Some)._0
            var inline227 string = "some " + inline225
            t210 = inline227
        default:
            panic("non-exhaustive match")
        }
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline222)
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
