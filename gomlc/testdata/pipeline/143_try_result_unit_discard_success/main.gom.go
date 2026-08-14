package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__unit__string interface {
    isResult__unit__string()
}

type Ok struct {
    _0 struct{}
}

func (_ Ok) isResult__unit__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__unit__string() {}

func step(ok__0 bool) Result__unit__string {
    if ok__0 {
        var t194 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t194
    } else {
        var t195 Result__unit__string = Err{
            _0: "step failed",
        }
        return t195
    }
}

func main0() struct{} {
    var t209 Result__unit__string
    var inline249 bool = true
    var inline250 Result__unit__string = step(inline249)
    switch inline250.(type) {
    case Ok:
        var inline253 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t209 = inline253
        var t210 string
        switch t209.(type) {
        case Ok:
            t210 = "ok unit"
        case Err:
            var inline245 string = t209.(Err)._0
            var inline247 string = "err " + inline245
            t210 = inline247
        default:
            panic("non-exhaustive match")
        }
        var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline241)
        var t211 Result__unit__string
        var inline230 bool = false
        var inline231 Result__unit__string = step(inline230)
        switch inline231.(type) {
        case Ok:
            var inline234 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t211 = inline234
            var t212 string
            switch t211.(type) {
            case Ok:
                t212 = "ok unit"
            case Err:
                var inline226 string = t211.(Err)._0
                var inline228 string = "err " + inline226
                t212 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
            _goml_runtime_core_string_println(inline222)
            return struct{}{}
        case Err:
            var inline237 string = inline231.(Err)._0
            var inline239 Result__unit__string = Err{
                _0: inline237,
            }
            t211 = inline239
            var t212 string
            switch t211.(type) {
            case Ok:
                t212 = "ok unit"
            case Err:
                var inline226 string = t211.(Err)._0
                var inline228 string = "err " + inline226
                t212 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
            _goml_runtime_core_string_println(inline222)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline256 string = inline250.(Err)._0
        var inline258 Result__unit__string = Err{
            _0: inline256,
        }
        t209 = inline258
        var t210 string
        switch t209.(type) {
        case Ok:
            t210 = "ok unit"
        case Err:
            var inline245 string = t209.(Err)._0
            var inline247 string = "err " + inline245
            t210 = inline247
        default:
            panic("non-exhaustive match")
        }
        var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline241)
        var t211 Result__unit__string
        var inline230 bool = false
        var inline231 Result__unit__string = step(inline230)
        switch inline231.(type) {
        case Ok:
            var inline234 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t211 = inline234
            var t212 string
            switch t211.(type) {
            case Ok:
                t212 = "ok unit"
            case Err:
                var inline226 string = t211.(Err)._0
                var inline228 string = "err " + inline226
                t212 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
            _goml_runtime_core_string_println(inline222)
            return struct{}{}
        case Err:
            var inline237 string = inline231.(Err)._0
            var inline239 Result__unit__string = Err{
                _0: inline237,
            }
            t211 = inline239
            var t212 string
            switch t211.(type) {
            case Ok:
                t212 = "ok unit"
            case Err:
                var inline226 string = t211.(Err)._0
                var inline228 string = "err " + inline226
                t212 = inline228
            default:
                panic("non-exhaustive match")
            }
            var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
            _goml_runtime_core_string_println(inline222)
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
