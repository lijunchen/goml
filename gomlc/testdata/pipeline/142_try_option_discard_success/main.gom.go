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
        var t182 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t183 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t182,
        }
        return t183
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func main0() struct{} {
    var t195 Option__string
    var inline231 bool = true
    var inline232 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline231)
    switch inline232.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t195 = Option__string_None{}
        var t196 string
        switch t195.(type) {
        case Option__string_None:
            t196 = "none"
        case Option__string_Some:
            var inline227 string = t195.(Option__string_Some)._0
            var inline229 string = "some " + inline227
            t196 = inline229
        default:
            panic("non-exhaustive match")
        }
        var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline224)
        var t197 Option__string
        var inline215 bool = false
        var inline216 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline215)
        switch inline216.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t197 = Option__string_None{}
            var t198 string
            switch t197.(type) {
            case Option__string_None:
                t198 = "none"
            case Option__string_Some:
                var inline211 string = t197.(Option__string_Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline220 Option__string = Option__string_Some{
                _0: "ok",
            }
            t197 = inline220
            var t198 string
            switch t197.(type) {
            case Option__string_None:
                t198 = "none"
            case Option__string_Some:
                var inline211 string = t197.(Option__string_Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline236 Option__string = Option__string_Some{
            _0: "ok",
        }
        t195 = inline236
        var t196 string
        switch t195.(type) {
        case Option__string_None:
            t196 = "none"
        case Option__string_Some:
            var inline227 string = t195.(Option__string_Some)._0
            var inline229 string = "some " + inline227
            t196 = inline229
        default:
            panic("non-exhaustive match")
        }
        var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline224)
        var t197 Option__string
        var inline215 bool = false
        var inline216 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline215)
        switch inline216.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t197 = Option__string_None{}
            var t198 string
            switch t197.(type) {
            case Option__string_None:
                t198 = "none"
            case Option__string_Some:
                var inline211 string = t197.(Option__string_Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline220 Option__string = Option__string_Some{
                _0: "ok",
            }
            t197 = inline220
            var t198 string
            switch t197.(type) {
            case Option__string_None:
                t198 = "none"
            case Option__string_Some:
                var inline211 string = t197.(Option__string_Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
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
