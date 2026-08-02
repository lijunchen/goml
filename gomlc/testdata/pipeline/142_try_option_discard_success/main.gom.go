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
        var t165 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t166 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t165,
        }
        return t166
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func main0() struct{} {
    var t178 Option__string
    var inline214 bool = true
    var inline215 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline214)
    switch inline215.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t178 = Option__string_None{}
        var t179 string
        switch t178.(type) {
        case Option__string_None:
            t179 = "none"
        case Option__string_Some:
            var inline210 string = t178.(Option__string_Some)._0
            var inline212 string = "some " + inline210
            t179 = inline212
        default:
            panic("non-exhaustive match")
        }
        var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
        _goml_runtime_core_string_println(inline207)
        var t180 Option__string
        var inline198 bool = false
        var inline199 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline198)
        switch inline199.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t180 = Option__string_None{}
            var t181 string
            switch t180.(type) {
            case Option__string_None:
                t181 = "none"
            case Option__string_Some:
                var inline194 string = t180.(Option__string_Some)._0
                var inline196 string = "some " + inline194
                t181 = inline196
            default:
                panic("non-exhaustive match")
            }
            var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline191)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline203 Option__string = Option__string_Some{
                _0: "ok",
            }
            t180 = inline203
            var t181 string
            switch t180.(type) {
            case Option__string_None:
                t181 = "none"
            case Option__string_Some:
                var inline194 string = t180.(Option__string_Some)._0
                var inline196 string = "some " + inline194
                t181 = inline196
            default:
                panic("non-exhaustive match")
            }
            var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline191)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline219 Option__string = Option__string_Some{
            _0: "ok",
        }
        t178 = inline219
        var t179 string
        switch t178.(type) {
        case Option__string_None:
            t179 = "none"
        case Option__string_Some:
            var inline210 string = t178.(Option__string_Some)._0
            var inline212 string = "some " + inline210
            t179 = inline212
        default:
            panic("non-exhaustive match")
        }
        var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
        _goml_runtime_core_string_println(inline207)
        var t180 Option__string
        var inline198 bool = false
        var inline199 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline198)
        switch inline199.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t180 = Option__string_None{}
            var t181 string
            switch t180.(type) {
            case Option__string_None:
                t181 = "none"
            case Option__string_Some:
                var inline194 string = t180.(Option__string_Some)._0
                var inline196 string = "some " + inline194
                t181 = inline196
            default:
                panic("non-exhaustive match")
            }
            var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline191)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline203 Option__string = Option__string_Some{
                _0: "ok",
            }
            t180 = inline203
            var t181 string
            switch t180.(type) {
            case Option__string_None:
                t181 = "none"
            case Option__string_Some:
                var inline194 string = t180.(Option__string_Some)._0
                var inline196 string = "some " + inline194
                t181 = inline196
            default:
                panic("non-exhaustive match")
            }
            var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
            _goml_runtime_core_string_println(inline191)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
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
