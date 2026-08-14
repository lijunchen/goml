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
        var t192 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t193 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t192,
        }
        return t193
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func main0() struct{} {
    var t205 Option__string
    var inline241 bool = true
    var inline242 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline241)
    switch inline242.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t205 = Option__string_None{}
        var t206 string
        switch t205.(type) {
        case Option__string_None:
            t206 = "none"
        case Option__string_Some:
            var inline237 string = t205.(Option__string_Some)._0
            var inline239 string = "some " + inline237
            t206 = inline239
        default:
            panic("non-exhaustive match")
        }
        var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
        _goml_runtime_core_string_println(inline234)
        var t207 Option__string
        var inline225 bool = false
        var inline226 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline225)
        switch inline226.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t207 = Option__string_None{}
            var t208 string
            switch t207.(type) {
            case Option__string_None:
                t208 = "none"
            case Option__string_Some:
                var inline221 string = t207.(Option__string_Some)._0
                var inline223 string = "some " + inline221
                t208 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline230 Option__string = Option__string_Some{
                _0: "ok",
            }
            t207 = inline230
            var t208 string
            switch t207.(type) {
            case Option__string_None:
                t208 = "none"
            case Option__string_Some:
                var inline221 string = t207.(Option__string_Some)._0
                var inline223 string = "some " + inline221
                t208 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline246 Option__string = Option__string_Some{
            _0: "ok",
        }
        t205 = inline246
        var t206 string
        switch t205.(type) {
        case Option__string_None:
            t206 = "none"
        case Option__string_Some:
            var inline237 string = t205.(Option__string_Some)._0
            var inline239 string = "some " + inline237
            t206 = inline239
        default:
            panic("non-exhaustive match")
        }
        var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
        _goml_runtime_core_string_println(inline234)
        var t207 Option__string
        var inline225 bool = false
        var inline226 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline225)
        switch inline226.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t207 = Option__string_None{}
            var t208 string
            switch t207.(type) {
            case Option__string_None:
                t208 = "none"
            case Option__string_Some:
                var inline221 string = t207.(Option__string_Some)._0
                var inline223 string = "some " + inline221
                t208 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline230 Option__string = Option__string_Some{
                _0: "ok",
            }
            t207 = inline230
            var t208 string
            switch t207.(type) {
            case Option__string_None:
                t208 = "none"
            case Option__string_Some:
                var inline221 string = t207.(Option__string_Some)._0
                var inline223 string = "some " + inline221
                t208 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline218)
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
