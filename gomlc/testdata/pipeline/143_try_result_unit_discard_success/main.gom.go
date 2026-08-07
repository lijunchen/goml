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
        var t184 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t184
    } else {
        var t185 Result__unit__string = Err{
            _0: "step failed",
        }
        return t185
    }
}

func main0() struct{} {
    var t199 Result__unit__string
    var inline239 bool = true
    var inline240 Result__unit__string = step(inline239)
    switch inline240.(type) {
    case Ok:
        var inline243 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t199 = inline243
        var t200 string
        switch t199.(type) {
        case Ok:
            t200 = "ok unit"
        case Err:
            var inline235 string = t199.(Err)._0
            var inline237 string = "err " + inline235
            t200 = inline237
        default:
            panic("non-exhaustive match")
        }
        var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
        _goml_runtime_core_string_println(inline231)
        var t201 Result__unit__string
        var inline220 bool = false
        var inline221 Result__unit__string = step(inline220)
        switch inline221.(type) {
        case Ok:
            var inline224 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t201 = inline224
            var t202 string
            switch t201.(type) {
            case Ok:
                t202 = "ok unit"
            case Err:
                var inline216 string = t201.(Err)._0
                var inline218 string = "err " + inline216
                t202 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
            _goml_runtime_core_string_println(inline212)
            return struct{}{}
        case Err:
            var inline227 string = inline221.(Err)._0
            var inline229 Result__unit__string = Err{
                _0: inline227,
            }
            t201 = inline229
            var t202 string
            switch t201.(type) {
            case Ok:
                t202 = "ok unit"
            case Err:
                var inline216 string = t201.(Err)._0
                var inline218 string = "err " + inline216
                t202 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
            _goml_runtime_core_string_println(inline212)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline246 string = inline240.(Err)._0
        var inline248 Result__unit__string = Err{
            _0: inline246,
        }
        t199 = inline248
        var t200 string
        switch t199.(type) {
        case Ok:
            t200 = "ok unit"
        case Err:
            var inline235 string = t199.(Err)._0
            var inline237 string = "err " + inline235
            t200 = inline237
        default:
            panic("non-exhaustive match")
        }
        var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
        _goml_runtime_core_string_println(inline231)
        var t201 Result__unit__string
        var inline220 bool = false
        var inline221 Result__unit__string = step(inline220)
        switch inline221.(type) {
        case Ok:
            var inline224 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t201 = inline224
            var t202 string
            switch t201.(type) {
            case Ok:
                t202 = "ok unit"
            case Err:
                var inline216 string = t201.(Err)._0
                var inline218 string = "err " + inline216
                t202 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
            _goml_runtime_core_string_println(inline212)
            return struct{}{}
        case Err:
            var inline227 string = inline221.(Err)._0
            var inline229 Result__unit__string = Err{
                _0: inline227,
            }
            t201 = inline229
            var t202 string
            switch t201.(type) {
            case Ok:
                t202 = "ok unit"
            case Err:
                var inline216 string = t201.(Err)._0
                var inline218 string = "err " + inline216
                t202 = inline218
            default:
                panic("non-exhaustive match")
            }
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
            _goml_runtime_core_string_println(inline212)
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
