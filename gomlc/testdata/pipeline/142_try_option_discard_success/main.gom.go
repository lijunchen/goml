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
        var t146 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t147 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t146,
        }
        return t147
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func main0() struct{} {
    var t159 Option__string
    var inline195 bool = true
    var inline196 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline195)
    switch inline196.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t159 = Option__string_None{}
        var t160 string
        switch t159.(type) {
        case Option__string_None:
            t160 = "none"
        case Option__string_Some:
            var inline191 string = t159.(Option__string_Some)._0
            var inline193 string = "some " + inline191
            t160 = inline193
        default:
            panic("non-exhaustive match")
        }
        var inline188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
        _goml_runtime_core_string_println(inline188)
        var t161 Option__string
        var inline179 bool = false
        var inline180 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline179)
        switch inline180.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t161 = Option__string_None{}
            var t162 string
            switch t161.(type) {
            case Option__string_None:
                t162 = "none"
            case Option__string_Some:
                var inline175 string = t161.(Option__string_Some)._0
                var inline177 string = "some " + inline175
                t162 = inline177
            default:
                panic("non-exhaustive match")
            }
            var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline172)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline184 Option__string = Option__string_Some{
                _0: "ok",
            }
            t161 = inline184
            var t162 string
            switch t161.(type) {
            case Option__string_None:
                t162 = "none"
            case Option__string_Some:
                var inline175 string = t161.(Option__string_Some)._0
                var inline177 string = "some " + inline175
                t162 = inline177
            default:
                panic("non-exhaustive match")
            }
            var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline172)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline200 Option__string = Option__string_Some{
            _0: "ok",
        }
        t159 = inline200
        var t160 string
        switch t159.(type) {
        case Option__string_None:
            t160 = "none"
        case Option__string_Some:
            var inline191 string = t159.(Option__string_Some)._0
            var inline193 string = "some " + inline191
            t160 = inline193
        default:
            panic("non-exhaustive match")
        }
        var inline188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
        _goml_runtime_core_string_println(inline188)
        var t161 Option__string
        var inline179 bool = false
        var inline180 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline179)
        switch inline180.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t161 = Option__string_None{}
            var t162 string
            switch t161.(type) {
            case Option__string_None:
                t162 = "none"
            case Option__string_Some:
                var inline175 string = t161.(Option__string_Some)._0
                var inline177 string = "some " + inline175
                t162 = inline177
            default:
                panic("non-exhaustive match")
            }
            var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline172)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline184 Option__string = Option__string_Some{
                _0: "ok",
            }
            t161 = inline184
            var t162 string
            switch t161.(type) {
            case Option__string_None:
                t162 = "none"
            case Option__string_Some:
                var inline175 string = t161.(Option__string_Some)._0
                var inline177 string = "some " + inline175
                t162 = inline177
            default:
                panic("non-exhaustive match")
            }
            var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline172)
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
