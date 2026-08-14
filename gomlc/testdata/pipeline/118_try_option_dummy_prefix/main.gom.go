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
    var t191 bool = case_id__0 == 0
    if t191 {
        var t192 Option__string = Some{
            _0: "ml",
        }
        return t192
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t205 Option__string
    var inline242 int32 = 0
    var inline243 Option__string = cut_prefix(inline242)
    var inline245 string
    switch inline243.(type) {
    case None:
        t205 = None{}
        var t206 string
        switch t205.(type) {
        case None:
            t206 = "none"
        case Some:
            var inline238 string = t205.(Some)._0
            var inline240 string = "some " + inline238
            t206 = inline240
        default:
            panic("non-exhaustive match")
        }
        var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
        _goml_runtime_core_string_println(inline235)
        var t207 Option__string
        var inline225 int32 = 1
        var inline226 Option__string = cut_prefix(inline225)
        var inline228 string
        switch inline226.(type) {
        case None:
            t207 = None{}
            var t208 string
            switch t207.(type) {
            case None:
                t208 = "none"
            case Some:
                var inline221 string = t207.(Some)._0
                var inline223 string = "some " + inline221
                t208 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        case Some:
            var inline232 string = inline226.(Some)._0
            inline228 = inline232
            var inline230 string = inline228 + "!"
            var inline231 Option__string = Some{
                _0: inline230,
            }
            t207 = inline231
            var t208 string
            switch t207.(type) {
            case None:
                t208 = "none"
            case Some:
                var inline221 string = t207.(Some)._0
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
    case Some:
        var inline249 string = inline243.(Some)._0
        inline245 = inline249
        var inline247 string = inline245 + "!"
        var inline248 Option__string = Some{
            _0: inline247,
        }
        t205 = inline248
        var t206 string
        switch t205.(type) {
        case None:
            t206 = "none"
        case Some:
            var inline238 string = t205.(Some)._0
            var inline240 string = "some " + inline238
            t206 = inline240
        default:
            panic("non-exhaustive match")
        }
        var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
        _goml_runtime_core_string_println(inline235)
        var t207 Option__string
        var inline225 int32 = 1
        var inline226 Option__string = cut_prefix(inline225)
        var inline228 string
        switch inline226.(type) {
        case None:
            t207 = None{}
            var t208 string
            switch t207.(type) {
            case None:
                t208 = "none"
            case Some:
                var inline221 string = t207.(Some)._0
                var inline223 string = "some " + inline221
                t208 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        case Some:
            var inline232 string = inline226.(Some)._0
            inline228 = inline232
            var inline230 string = inline228 + "!"
            var inline231 Option__string = Some{
                _0: inline230,
            }
            t207 = inline231
            var t208 string
            switch t207.(type) {
            case None:
                t208 = "none"
            case Some:
                var inline221 string = t207.(Some)._0
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
