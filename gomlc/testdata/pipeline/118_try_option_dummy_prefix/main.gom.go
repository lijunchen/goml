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
    var t181 bool
    var inline208 int32 = 0
    var inline209 bool = case_id__0 == inline208
    t181 = inline209
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
    var inline238 int32 = 0
    var inline239 Option__string = cut_prefix(inline238)
    var inline241 string
    switch inline239.(type) {
    case None:
        t195 = None{}
        var t196 string
        switch t195.(type) {
        case None:
            t196 = "none"
        case Some:
            var inline234 string = t195.(Some)._0
            var inline236 string = "some " + inline234
            t196 = inline236
        default:
            panic("non-exhaustive match")
        }
        var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline231)
        var t197 Option__string
        var inline221 int32 = 1
        var inline222 Option__string = cut_prefix(inline221)
        var inline224 string
        switch inline222.(type) {
        case None:
            t197 = None{}
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline217 string = t197.(Some)._0
                var inline219 string = "some " + inline217
                t198 = inline219
            default:
                panic("non-exhaustive match")
            }
            var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline214)
            return struct{}{}
        case Some:
            var inline228 string = inline222.(Some)._0
            inline224 = inline228
            var inline226 string = inline224 + "!"
            var inline227 Option__string = Some{
                _0: inline226,
            }
            t197 = inline227
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline217 string = t197.(Some)._0
                var inline219 string = "some " + inline217
                t198 = inline219
            default:
                panic("non-exhaustive match")
            }
            var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline214)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline245 string = inline239.(Some)._0
        inline241 = inline245
        var inline243 string = inline241 + "!"
        var inline244 Option__string = Some{
            _0: inline243,
        }
        t195 = inline244
        var t196 string
        switch t195.(type) {
        case None:
            t196 = "none"
        case Some:
            var inline234 string = t195.(Some)._0
            var inline236 string = "some " + inline234
            t196 = inline236
        default:
            panic("non-exhaustive match")
        }
        var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline231)
        var t197 Option__string
        var inline221 int32 = 1
        var inline222 Option__string = cut_prefix(inline221)
        var inline224 string
        switch inline222.(type) {
        case None:
            t197 = None{}
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline217 string = t197.(Some)._0
                var inline219 string = "some " + inline217
                t198 = inline219
            default:
                panic("non-exhaustive match")
            }
            var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline214)
            return struct{}{}
        case Some:
            var inline228 string = inline222.(Some)._0
            inline224 = inline228
            var inline226 string = inline224 + "!"
            var inline227 Option__string = Some{
                _0: inline226,
            }
            t197 = inline227
            var t198 string
            switch t197.(type) {
            case None:
                t198 = "none"
            case Some:
                var inline217 string = t197.(Some)._0
                var inline219 string = "some " + inline217
                t198 = inline219
            default:
                panic("non-exhaustive match")
            }
            var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline214)
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
