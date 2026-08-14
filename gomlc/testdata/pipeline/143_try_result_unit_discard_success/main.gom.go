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
        var t199 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t199
    } else {
        var t200 Result__unit__string = Err{
            _0: "step failed",
        }
        return t200
    }
}

func main0() struct{} {
    var t214 Result__unit__string
    var inline254 bool = true
    var inline255 Result__unit__string = step(inline254)
    switch inline255.(type) {
    case Ok:
        var inline258 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t214 = inline258
        var t215 string
        switch t214.(type) {
        case Ok:
            t215 = "ok unit"
        case Err:
            var inline250 string = t214.(Err)._0
            var inline252 string = "err " + inline250
            t215 = inline252
        default:
            panic("non-exhaustive match")
        }
        var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline246)
        var t216 Result__unit__string
        var inline235 bool = false
        var inline236 Result__unit__string = step(inline235)
        switch inline236.(type) {
        case Ok:
            var inline239 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t216 = inline239
            var t217 string
            switch t216.(type) {
            case Ok:
                t217 = "ok unit"
            case Err:
                var inline231 string = t216.(Err)._0
                var inline233 string = "err " + inline231
                t217 = inline233
            default:
                panic("non-exhaustive match")
            }
            var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
            _goml_runtime_core_string_println(inline227)
            return struct{}{}
        case Err:
            var inline242 string = inline236.(Err)._0
            var inline244 Result__unit__string = Err{
                _0: inline242,
            }
            t216 = inline244
            var t217 string
            switch t216.(type) {
            case Ok:
                t217 = "ok unit"
            case Err:
                var inline231 string = t216.(Err)._0
                var inline233 string = "err " + inline231
                t217 = inline233
            default:
                panic("non-exhaustive match")
            }
            var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
            _goml_runtime_core_string_println(inline227)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline261 string = inline255.(Err)._0
        var inline263 Result__unit__string = Err{
            _0: inline261,
        }
        t214 = inline263
        var t215 string
        switch t214.(type) {
        case Ok:
            t215 = "ok unit"
        case Err:
            var inline250 string = t214.(Err)._0
            var inline252 string = "err " + inline250
            t215 = inline252
        default:
            panic("non-exhaustive match")
        }
        var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline246)
        var t216 Result__unit__string
        var inline235 bool = false
        var inline236 Result__unit__string = step(inline235)
        switch inline236.(type) {
        case Ok:
            var inline239 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t216 = inline239
            var t217 string
            switch t216.(type) {
            case Ok:
                t217 = "ok unit"
            case Err:
                var inline231 string = t216.(Err)._0
                var inline233 string = "err " + inline231
                t217 = inline233
            default:
                panic("non-exhaustive match")
            }
            var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
            _goml_runtime_core_string_println(inline227)
            return struct{}{}
        case Err:
            var inline242 string = inline236.(Err)._0
            var inline244 Result__unit__string = Err{
                _0: inline242,
            }
            t216 = inline244
            var t217 string
            switch t216.(type) {
            case Ok:
                t217 = "ok unit"
            case Err:
                var inline231 string = t216.(Err)._0
                var inline233 string = "err " + inline231
                t217 = inline233
            default:
                panic("non-exhaustive match")
            }
            var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
            _goml_runtime_core_string_println(inline227)
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
