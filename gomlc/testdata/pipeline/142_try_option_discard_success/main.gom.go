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
        var t197 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t198 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t197,
        }
        return t198
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func main0() struct{} {
    var t210 Option__string
    var inline246 bool = true
    var inline247 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline246)
    switch inline247.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t210 = Option__string_None{}
        var t211 string
        switch t210.(type) {
        case Option__string_None:
            t211 = "none"
        case Option__string_Some:
            var inline242 string = t210.(Option__string_Some)._0
            var inline244 string = "some " + inline242
            t211 = inline244
        default:
            panic("non-exhaustive match")
        }
        var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
        _goml_runtime_core_string_println(inline239)
        var t212 Option__string
        var inline230 bool = false
        var inline231 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline230)
        switch inline231.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t212 = Option__string_None{}
            var t213 string
            switch t212.(type) {
            case Option__string_None:
                t213 = "none"
            case Option__string_Some:
                var inline226 string = t212.(Option__string_Some)._0
                var inline228 string = "some " + inline226
                t213 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline235 Option__string = Option__string_Some{
                _0: "ok",
            }
            t212 = inline235
            var t213 string
            switch t212.(type) {
            case Option__string_None:
                t213 = "none"
            case Option__string_Some:
                var inline226 string = t212.(Option__string_Some)._0
                var inline228 string = "some " + inline226
                t213 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline251 Option__string = Option__string_Some{
            _0: "ok",
        }
        t210 = inline251
        var t211 string
        switch t210.(type) {
        case Option__string_None:
            t211 = "none"
        case Option__string_Some:
            var inline242 string = t210.(Option__string_Some)._0
            var inline244 string = "some " + inline242
            t211 = inline244
        default:
            panic("non-exhaustive match")
        }
        var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
        _goml_runtime_core_string_println(inline239)
        var t212 Option__string
        var inline230 bool = false
        var inline231 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline230)
        switch inline231.(type) {
        case _goml_m_Option_____o_string_c_string_q__None:
            t212 = Option__string_None{}
            var t213 string
            switch t212.(type) {
            case Option__string_None:
                t213 = "none"
            case Option__string_Some:
                var inline226 string = t212.(Option__string_Some)._0
                var inline228 string = "some " + inline226
                t213 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case _goml_m_Option_____o_string_c_string_q__Some:
            var inline235 Option__string = Option__string_Some{
                _0: "ok",
            }
            t212 = inline235
            var t213 string
            switch t212.(type) {
            case Option__string_None:
                t213 = "none"
            case Option__string_Some:
                var inline226 string = t212.(Option__string_Some)._0
                var inline228 string = "some " + inline226
                t213 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline223)
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
