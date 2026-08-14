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
        var t194 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t195 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t194,
        }
        return t195
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp182 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline219 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline220 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline219,
        }
        mtmp182 = inline220
    } else {
        mtmp182 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp199 Tuple2_6string_6string
    switch mtmp182.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x183 Tuple2_6string_6string = mtmp182.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp199 = x183
        var x185 string = jp199._0
        var x186 string = jp199._1
        var t200 string = x185 + "|"
        var t201 string = t200 + x186
        var t202 Option__string = Option__string_Some{
            _0: t201,
        }
        return t202
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t209 Option__string = describe(true)
    var t210 string
    switch t209.(type) {
    case Option__string_None:
        t210 = "none"
    case Option__string_Some:
        var inline247 string = t209.(Option__string_Some)._0
        var inline249 string = "some " + inline247
        t210 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline244)
    var t211 Option__string
    var inline229 bool = false
    var inline230 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline229)
    var inline232 Tuple2_6string_6string
    switch inline230.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t211 = Option__string_None{}
        var t212 string
        switch t211.(type) {
        case Option__string_None:
            t212 = "none"
        case Option__string_Some:
            var inline225 string = t211.(Option__string_Some)._0
            var inline227 string = "some " + inline225
            t212 = inline227
        default:
            panic("non-exhaustive match")
        }
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
        _goml_runtime_core_string_println(inline222)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline241 Tuple2_6string_6string = inline230.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline232 = inline241
        var inline234 string = inline232._0
        var inline235 string = inline232._1
        var inline238 string = inline234 + "|"
        var inline239 string = inline238 + inline235
        var inline240 Option__string = Option__string_Some{
            _0: inline239,
        }
        t211 = inline240
        var t212 string
        switch t211.(type) {
        case Option__string_None:
            t212 = "none"
        case Option__string_Some:
            var inline225 string = t211.(Option__string_Some)._0
            var inline227 string = "some " + inline225
            t212 = inline227
        default:
            panic("non-exhaustive match")
        }
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
        _goml_runtime_core_string_println(inline222)
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
