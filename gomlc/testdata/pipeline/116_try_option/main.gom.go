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

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t191 Option__int32 = Some{
            _0: 4,
        }
        return t191
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t194 int32 = a__1 + b__2
    return t194
}

func main0() struct{} {
    var t208 Option__int32
    var inline253 bool = true
    var inline254 Option__int32 = maybe_value(inline253)
    var inline256 int32
    switch inline254.(type) {
    case None:
        t208 = None{}
        var t209 string
        switch t208.(type) {
        case None:
            t209 = "none"
        case Some:
            var inline248 int32 = t208.(Some)._0
            var inline250 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline248)
            var inline251 string = "some=" + inline250
            t209 = inline251
        default:
            panic("non-exhaustive match")
        }
        var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
        _goml_runtime_core_string_println(inline245)
        var t210 Option__int32
        var inline236 bool = false
        var inline237 Option__int32 = maybe_value(inline236)
        var inline239 int32
        switch inline237.(type) {
        case None:
            t210 = None{}
            var t211 string
            switch t210.(type) {
            case None:
                t211 = "none"
            case Some:
                var inline231 int32 = t210.(Some)._0
                var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
                var inline234 string = "some=" + inline233
                t211 = inline234
            default:
                panic("non-exhaustive match")
            }
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        case Some:
            var inline242 int32 = inline237.(Some)._0
            inline239 = inline242
            var inline240 int32 = add(inline239, 2)
            var inline241 Option__int32 = Some{
                _0: inline240,
            }
            t210 = inline241
            var t211 string
            switch t210.(type) {
            case None:
                t211 = "none"
            case Some:
                var inline231 int32 = t210.(Some)._0
                var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
                var inline234 string = "some=" + inline233
                t211 = inline234
            default:
                panic("non-exhaustive match")
            }
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline259 int32 = inline254.(Some)._0
        inline256 = inline259
        var inline257 int32 = add(inline256, 2)
        var inline258 Option__int32 = Some{
            _0: inline257,
        }
        t208 = inline258
        var t209 string
        switch t208.(type) {
        case None:
            t209 = "none"
        case Some:
            var inline248 int32 = t208.(Some)._0
            var inline250 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline248)
            var inline251 string = "some=" + inline250
            t209 = inline251
        default:
            panic("non-exhaustive match")
        }
        var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
        _goml_runtime_core_string_println(inline245)
        var t210 Option__int32
        var inline236 bool = false
        var inline237 Option__int32 = maybe_value(inline236)
        var inline239 int32
        switch inline237.(type) {
        case None:
            t210 = None{}
            var t211 string
            switch t210.(type) {
            case None:
                t211 = "none"
            case Some:
                var inline231 int32 = t210.(Some)._0
                var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
                var inline234 string = "some=" + inline233
                t211 = inline234
            default:
                panic("non-exhaustive match")
            }
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        case Some:
            var inline242 int32 = inline237.(Some)._0
            inline239 = inline242
            var inline240 int32 = add(inline239, 2)
            var inline241 Option__int32 = Some{
                _0: inline240,
            }
            t210 = inline241
            var t211 string
            switch t210.(type) {
            case None:
                t211 = "none"
            case Some:
                var inline231 int32 = t210.(Some)._0
                var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
                var inline234 string = "some=" + inline233
                t211 = inline234
            default:
                panic("non-exhaustive match")
            }
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t214 string = _goml_runtime_core_int32_to_string(self__33)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
