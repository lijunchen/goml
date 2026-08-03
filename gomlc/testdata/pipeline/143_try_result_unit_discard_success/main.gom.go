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
        var t189 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t189
    } else {
        var t190 Result__unit__string = Err{
            _0: "step failed",
        }
        return t190
    }
}

func main0() struct{} {
    var t204 Result__unit__string
    var inline244 bool = true
    var inline245 Result__unit__string = step(inline244)
    switch inline245.(type) {
    case Ok:
        var inline248 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t204 = inline248
        var t205 string
        switch t204.(type) {
        case Ok:
            t205 = "ok unit"
        case Err:
            var inline240 string = t204.(Err)._0
            var inline242 string = "err " + inline240
            t205 = inline242
        default:
            panic("non-exhaustive match")
        }
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline236)
        var t206 Result__unit__string
        var inline225 bool = false
        var inline226 Result__unit__string = step(inline225)
        switch inline226.(type) {
        case Ok:
            var inline229 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t206 = inline229
            var t207 string
            switch t206.(type) {
            case Ok:
                t207 = "ok unit"
            case Err:
                var inline221 string = t206.(Err)._0
                var inline223 string = "err " + inline221
                t207 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
            _goml_runtime_core_string_println(inline217)
            return struct{}{}
        case Err:
            var inline232 string = inline226.(Err)._0
            var inline234 Result__unit__string = Err{
                _0: inline232,
            }
            t206 = inline234
            var t207 string
            switch t206.(type) {
            case Ok:
                t207 = "ok unit"
            case Err:
                var inline221 string = t206.(Err)._0
                var inline223 string = "err " + inline221
                t207 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
            _goml_runtime_core_string_println(inline217)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline251 string = inline245.(Err)._0
        var inline253 Result__unit__string = Err{
            _0: inline251,
        }
        t204 = inline253
        var t205 string
        switch t204.(type) {
        case Ok:
            t205 = "ok unit"
        case Err:
            var inline240 string = t204.(Err)._0
            var inline242 string = "err " + inline240
            t205 = inline242
        default:
            panic("non-exhaustive match")
        }
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline236)
        var t206 Result__unit__string
        var inline225 bool = false
        var inline226 Result__unit__string = step(inline225)
        switch inline226.(type) {
        case Ok:
            var inline229 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t206 = inline229
            var t207 string
            switch t206.(type) {
            case Ok:
                t207 = "ok unit"
            case Err:
                var inline221 string = t206.(Err)._0
                var inline223 string = "err " + inline221
                t207 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
            _goml_runtime_core_string_println(inline217)
            return struct{}{}
        case Err:
            var inline232 string = inline226.(Err)._0
            var inline234 Result__unit__string = Err{
                _0: inline232,
            }
            t206 = inline234
            var t207 string
            switch t206.(type) {
            case Ok:
                t207 = "ok unit"
            case Err:
                var inline221 string = t206.(Err)._0
                var inline223 string = "err " + inline221
                t207 = inline223
            default:
                panic("non-exhaustive match")
            }
            var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
            _goml_runtime_core_string_println(inline217)
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
