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
        var t195 Option__int32 = Some{
            _0: 41,
        }
        return t195
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t208 Option__int32
    var inline250 bool = true
    var inline251 Option__int32 = maybe_value(inline250)
    var inline253 int32
    switch inline251.(type) {
    case Some:
        var inline257 int32 = inline251.(Some)._0
        inline253 = inline257
        var inline255 int32 = inline253 + 1
        var inline256 Option__int32 = Some{
            _0: inline255,
        }
        t208 = inline256
        var t209 string
        switch t208.(type) {
        case Some:
            var inline246 int32 = t208.(Some)._0
            var inline248 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline246)
            t209 = inline248
        case None:
            t209 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
        _goml_runtime_core_string_println(inline243)
        var t210 Option__int32
        var inline233 bool = false
        var inline234 Option__int32 = maybe_value(inline233)
        var inline236 int32
        switch inline234.(type) {
        case Some:
            var inline240 int32 = inline234.(Some)._0
            inline236 = inline240
            var inline238 int32 = inline236 + 1
            var inline239 Option__int32 = Some{
                _0: inline238,
            }
            t210 = inline239
            var t211 string
            switch t210.(type) {
            case Some:
                var inline229 int32 = t210.(Some)._0
                var inline231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline229)
                t211 = inline231
            case None:
                t211 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline226)
            return struct{}{}
        case None:
            t210 = None{}
            var t211 string
            switch t210.(type) {
            case Some:
                var inline229 int32 = t210.(Some)._0
                var inline231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline229)
                t211 = inline231
            case None:
                t211 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline226)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t208 = None{}
        var t209 string
        switch t208.(type) {
        case Some:
            var inline246 int32 = t208.(Some)._0
            var inline248 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline246)
            t209 = inline248
        case None:
            t209 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
        _goml_runtime_core_string_println(inline243)
        var t210 Option__int32
        var inline233 bool = false
        var inline234 Option__int32 = maybe_value(inline233)
        var inline236 int32
        switch inline234.(type) {
        case Some:
            var inline240 int32 = inline234.(Some)._0
            inline236 = inline240
            var inline238 int32 = inline236 + 1
            var inline239 Option__int32 = Some{
                _0: inline238,
            }
            t210 = inline239
            var t211 string
            switch t210.(type) {
            case Some:
                var inline229 int32 = t210.(Some)._0
                var inline231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline229)
                t211 = inline231
            case None:
                t211 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline226)
            return struct{}{}
        case None:
            t210 = None{}
            var t211 string
            switch t210.(type) {
            case Some:
                var inline229 int32 = t210.(Some)._0
                var inline231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline229)
                t211 = inline231
            case None:
                t211 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline226)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t215 string = _goml_runtime_core_int32_to_string(self__33)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
