package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__int32 interface {
    isOption__int32()
}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type None struct {}

func (_ None) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t185 Option__int32 = Some{
            _0: 41,
        }
        return t185
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t198 Option__int32
    var inline240 bool = true
    var inline241 Option__int32 = maybe_value(inline240)
    var inline243 int32
    switch inline241.(type) {
    case Some:
        var inline247 int32 = inline241.(Some)._0
        inline243 = inline247
        var inline245 int32 = inline243 + 1
        var inline246 Option__int32 = Some{
            _0: inline245,
        }
        t198 = inline246
        var t199 string
        switch t198.(type) {
        case Some:
            var inline236 int32 = t198.(Some)._0
            var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
            t199 = inline238
        case None:
            t199 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
        _goml_runtime_core_string_println(inline233)
        var t200 Option__int32
        var inline223 bool = false
        var inline224 Option__int32 = maybe_value(inline223)
        var inline226 int32
        switch inline224.(type) {
        case Some:
            var inline230 int32 = inline224.(Some)._0
            inline226 = inline230
            var inline228 int32 = inline226 + 1
            var inline229 Option__int32 = Some{
                _0: inline228,
            }
            t200 = inline229
            var t201 string
            switch t200.(type) {
            case Some:
                var inline219 int32 = t200.(Some)._0
                var inline221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline219)
                t201 = inline221
            case None:
                t201 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline216)
            return struct{}{}
        case None:
            t200 = None{}
            var t201 string
            switch t200.(type) {
            case Some:
                var inline219 int32 = t200.(Some)._0
                var inline221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline219)
                t201 = inline221
            case None:
                t201 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline216)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t198 = None{}
        var t199 string
        switch t198.(type) {
        case Some:
            var inline236 int32 = t198.(Some)._0
            var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
            t199 = inline238
        case None:
            t199 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
        _goml_runtime_core_string_println(inline233)
        var t200 Option__int32
        var inline223 bool = false
        var inline224 Option__int32 = maybe_value(inline223)
        var inline226 int32
        switch inline224.(type) {
        case Some:
            var inline230 int32 = inline224.(Some)._0
            inline226 = inline230
            var inline228 int32 = inline226 + 1
            var inline229 Option__int32 = Some{
                _0: inline228,
            }
            t200 = inline229
            var t201 string
            switch t200.(type) {
            case Some:
                var inline219 int32 = t200.(Some)._0
                var inline221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline219)
                t201 = inline221
            case None:
                t201 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline216)
            return struct{}{}
        case None:
            t200 = None{}
            var t201 string
            switch t200.(type) {
            case Some:
                var inline219 int32 = t200.(Some)._0
                var inline221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline219)
                t201 = inline221
            case None:
                t201 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline216)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t205 string = _goml_runtime_core_int32_to_string(self__35)
    return t205
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
