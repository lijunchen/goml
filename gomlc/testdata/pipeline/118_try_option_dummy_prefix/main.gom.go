package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func cut_prefix(case_id__0 int32) Option__string {
    var t196 bool = case_id__0 == 0
    if t196 {
        var t197 Option__string = Some{
            _0: "ml",
        }
        return t197
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t210 Option__string
    var inline247 int32 = 0
    var inline248 Option__string = cut_prefix(inline247)
    var inline250 string
    switch inline248.(type) {
    case None:
        t210 = None{}
        var t211 string
        switch t210.(type) {
        case None:
            t211 = "none"
        case Some:
            var inline243 string = t210.(Some)._0
            var inline245 string = "some " + inline243
            t211 = inline245
        default:
            panic("non-exhaustive match")
        }
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
        _goml_runtime_core_string_println(inline240)
        var t212 Option__string
        var inline230 int32 = 1
        var inline231 Option__string = cut_prefix(inline230)
        var inline233 string
        switch inline231.(type) {
        case None:
            t212 = None{}
            var t213 string
            switch t212.(type) {
            case None:
                t213 = "none"
            case Some:
                var inline226 string = t212.(Some)._0
                var inline228 string = "some " + inline226
                t213 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case Some:
            var inline237 string = inline231.(Some)._0
            inline233 = inline237
            var inline235 string = inline233 + "!"
            var inline236 Option__string = Some{
                _0: inline235,
            }
            t212 = inline236
            var t213 string
            switch t212.(type) {
            case None:
                t213 = "none"
            case Some:
                var inline226 string = t212.(Some)._0
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
    case Some:
        var inline254 string = inline248.(Some)._0
        inline250 = inline254
        var inline252 string = inline250 + "!"
        var inline253 Option__string = Some{
            _0: inline252,
        }
        t210 = inline253
        var t211 string
        switch t210.(type) {
        case None:
            t211 = "none"
        case Some:
            var inline243 string = t210.(Some)._0
            var inline245 string = "some " + inline243
            t211 = inline245
        default:
            panic("non-exhaustive match")
        }
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
        _goml_runtime_core_string_println(inline240)
        var t212 Option__string
        var inline230 int32 = 1
        var inline231 Option__string = cut_prefix(inline230)
        var inline233 string
        switch inline231.(type) {
        case None:
            t212 = None{}
            var t213 string
            switch t212.(type) {
            case None:
                t213 = "none"
            case Some:
                var inline226 string = t212.(Some)._0
                var inline228 string = "some " + inline226
                t213 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case Some:
            var inline237 string = inline231.(Some)._0
            inline233 = inline237
            var inline235 string = inline233 + "!"
            var inline236 Option__string = Some{
                _0: inline235,
            }
            t212 = inline236
            var t213 string
            switch t212.(type) {
            case None:
                t213 = "none"
            case Some:
                var inline226 string = t212.(Some)._0
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
