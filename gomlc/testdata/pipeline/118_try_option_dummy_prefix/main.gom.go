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
    var t181 bool = case_id__0 == 0
    if t181 {
        var t182 Option__string = Some{
            _0: "ml",
        }
        return t182
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t195 Option__string
    var inline232 int32 = 0
    var inline233 Option__string = cut_prefix(inline232)
    var inline235 string
    switch inline233.(type) {
    case None:
        t195 = None{}
        var t196 string
        switch t195.(type) {
        case None:
            t196 = "none"
        case Some:
            var inline228 string = t195.(Some)._0
            var inline230 string = "some " + inline228
            t196 = inline230
        default:
            panic("non-exhaustive match")
        }
        var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline225)
        var t197 Option__string
        var inline215 int32 = 1
        var inline216 Option__string = cut_prefix(inline215)
        var inline218 string
        switch inline216.(type) {
        case None:
            t197 = None{}
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline211 string = t197.(Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        case Some:
            var inline222 string = inline216.(Some)._0
            inline218 = inline222
            var inline220 string = inline218 + "!"
            var inline221 Option__string = Some{
                _0: inline220,
            }
            t197 = inline221
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline211 string = t197.(Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline239 string = inline233.(Some)._0
        inline235 = inline239
        var inline237 string = inline235 + "!"
        var inline238 Option__string = Some{
            _0: inline237,
        }
        t195 = inline238
        var t196 string
        switch t195.(type) {
        case None:
            t196 = "none"
        case Some:
            var inline228 string = t195.(Some)._0
            var inline230 string = "some " + inline228
            t196 = inline230
        default:
            panic("non-exhaustive match")
        }
        var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline225)
        var t197 Option__string
        var inline215 int32 = 1
        var inline216 Option__string = cut_prefix(inline215)
        var inline218 string
        switch inline216.(type) {
        case None:
            t197 = None{}
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline211 string = t197.(Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
            return struct{}{}
        case Some:
            var inline222 string = inline216.(Some)._0
            inline218 = inline222
            var inline220 string = inline218 + "!"
            var inline221 Option__string = Some{
                _0: inline220,
            }
            t197 = inline221
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline211 string = t197.(Some)._0
                var inline213 string = "some " + inline211
                t198 = inline213
            default:
                panic("non-exhaustive match")
            }
            var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline208)
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
