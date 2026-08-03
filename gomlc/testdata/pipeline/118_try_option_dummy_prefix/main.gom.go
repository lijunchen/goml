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
    var t186 bool
    var inline213 int32 = 0
    var inline214 bool = case_id__0 == inline213
    t186 = inline214
    if t186 {
        var t187 Option__string = Some{
            _0: "ml",
        }
        return t187
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t200 Option__string
    var inline243 int32 = 0
    var inline244 Option__string = cut_prefix(inline243)
    var inline246 string
    switch inline244.(type) {
    case None:
        t200 = None{}
        var t201 string
        switch t200.(type) {
        case None:
            t201 = "none"
        case Some:
            var inline239 string = t200.(Some)._0
            var inline241 string = "some " + inline239
            t201 = inline241
        default:
            panic("non-exhaustive match")
        }
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
        _goml_runtime_core_string_println(inline236)
        var t202 Option__string
        var inline226 int32 = 1
        var inline227 Option__string = cut_prefix(inline226)
        var inline229 string
        switch inline227.(type) {
        case None:
            t202 = None{}
            var t203 string
            switch t202.(type) {
            case None:
                t203 = "none"
            case Some:
                var inline222 string = t202.(Some)._0
                var inline224 string = "some " + inline222
                t203 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline219)
            return struct{}{}
        case Some:
            var inline233 string = inline227.(Some)._0
            inline229 = inline233
            var inline231 string = inline229 + "!"
            var inline232 Option__string = Some{
                _0: inline231,
            }
            t202 = inline232
            var t203 string
            switch t202.(type) {
            case None:
                t203 = "none"
            case Some:
                var inline222 string = t202.(Some)._0
                var inline224 string = "some " + inline222
                t203 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline219)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline250 string = inline244.(Some)._0
        inline246 = inline250
        var inline248 string = inline246 + "!"
        var inline249 Option__string = Some{
            _0: inline248,
        }
        t200 = inline249
        var t201 string
        switch t200.(type) {
        case None:
            t201 = "none"
        case Some:
            var inline239 string = t200.(Some)._0
            var inline241 string = "some " + inline239
            t201 = inline241
        default:
            panic("non-exhaustive match")
        }
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
        _goml_runtime_core_string_println(inline236)
        var t202 Option__string
        var inline226 int32 = 1
        var inline227 Option__string = cut_prefix(inline226)
        var inline229 string
        switch inline227.(type) {
        case None:
            t202 = None{}
            var t203 string
            switch t202.(type) {
            case None:
                t203 = "none"
            case Some:
                var inline222 string = t202.(Some)._0
                var inline224 string = "some " + inline222
                t203 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline219)
            return struct{}{}
        case Some:
            var inline233 string = inline227.(Some)._0
            inline229 = inline233
            var inline231 string = inline229 + "!"
            var inline232 Option__string = Some{
                _0: inline231,
            }
            t202 = inline232
            var t203 string
            switch t202.(type) {
            case None:
                t203 = "none"
            case Some:
                var inline222 string = t202.(Some)._0
                var inline224 string = "some " + inline222
                t203 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline219)
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
