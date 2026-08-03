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
        var t187 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t188 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t187,
        }
        return t188
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func main0() struct{} {
    var t200 Option__string
    var inline236 bool = true
    var inline237 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline236)
    switch inline237.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t200 = Option__string_None{}
        var t201 string
        switch t200.(type) {
        case Option__string_None:
            t201 = "none"
        case Option__string_Some:
            var inline232 string = t200.(Option__string_Some)._0
            var inline234 string = "some " + inline232
            t201 = inline234
        default:
            panic("non-exhaustive match")
        }
        var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
        _goml_runtime_core_string_println(inline229)
        var t202 Option__string
        var inline220 bool = false
        var inline221 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline220)
        switch inline221.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t202 = Option__string_None{}
            var t203 string
            switch t202.(type) {
            case Option__string_None:
                t203 = "none"
            case Option__string_Some:
                var inline216 string = t202.(Option__string_Some)._0
                var inline218 string = "some " + inline216
                t203 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline213)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline225 Option__string = Option__string_Some{
                _0: "ok",
            }
            t202 = inline225
            var t203 string
            switch t202.(type) {
            case Option__string_None:
                t203 = "none"
            case Option__string_Some:
                var inline216 string = t202.(Option__string_Some)._0
                var inline218 string = "some " + inline216
                t203 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline213)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline241 Option__string = Option__string_Some{
            _0: "ok",
        }
        t200 = inline241
        var t201 string
        switch t200.(type) {
        case Option__string_None:
            t201 = "none"
        case Option__string_Some:
            var inline232 string = t200.(Option__string_Some)._0
            var inline234 string = "some " + inline232
            t201 = inline234
        default:
            panic("non-exhaustive match")
        }
        var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
        _goml_runtime_core_string_println(inline229)
        var t202 Option__string
        var inline220 bool = false
        var inline221 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline220)
        switch inline221.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t202 = Option__string_None{}
            var t203 string
            switch t202.(type) {
            case Option__string_None:
                t203 = "none"
            case Option__string_Some:
                var inline216 string = t202.(Option__string_Some)._0
                var inline218 string = "some " + inline216
                t203 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline213)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline225 Option__string = Option__string_Some{
                _0: "ok",
            }
            t202 = inline225
            var t203 string
            switch t202.(type) {
            case Option__string_None:
                t203 = "none"
            case Option__string_Some:
                var inline216 string = t202.(Option__string_Some)._0
                var inline218 string = "some " + inline216
                t203 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline213)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
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
