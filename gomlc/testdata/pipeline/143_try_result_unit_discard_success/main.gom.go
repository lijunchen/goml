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
        var t167 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t167
    } else {
        var t168 Result__unit__string = Err{
            _0: "step failed",
        }
        return t168
    }
}

func main0() struct{} {
    var t182 Result__unit__string
    var inline222 bool = true
    var inline223 Result__unit__string = step(inline222)
    switch inline223.(type) {
    case Ok:
        var inline226 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t182 = inline226
        var t183 string
        switch t182.(type) {
        case Ok:
            t183 = "ok unit"
        case Err:
            var inline218 string = t182.(Err)._0
            var inline220 string = "err " + inline218
            t183 = inline220
        default:
            panic("non-exhaustive match")
        }
        var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
        _goml_runtime_core_string_println(inline214)
        var t184 Result__unit__string
        var inline203 bool = false
        var inline204 Result__unit__string = step(inline203)
        switch inline204.(type) {
        case Ok:
            var inline207 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t184 = inline207
            var t185 string
            switch t184.(type) {
            case Ok:
                t185 = "ok unit"
            case Err:
                var inline199 string = t184.(Err)._0
                var inline201 string = "err " + inline199
                t185 = inline201
            default:
                panic("non-exhaustive match")
            }
            var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
            _goml_runtime_core_string_println(inline195)
            return struct{}{}
        case Err:
            var inline210 string = inline204.(Err)._0
            var inline212 Result__unit__string = Err{
                _0: inline210,
            }
            t184 = inline212
            var t185 string
            switch t184.(type) {
            case Ok:
                t185 = "ok unit"
            case Err:
                var inline199 string = t184.(Err)._0
                var inline201 string = "err " + inline199
                t185 = inline201
            default:
                panic("non-exhaustive match")
            }
            var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
            _goml_runtime_core_string_println(inline195)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline229 string = inline223.(Err)._0
        var inline231 Result__unit__string = Err{
            _0: inline229,
        }
        t182 = inline231
        var t183 string
        switch t182.(type) {
        case Ok:
            t183 = "ok unit"
        case Err:
            var inline218 string = t182.(Err)._0
            var inline220 string = "err " + inline218
            t183 = inline220
        default:
            panic("non-exhaustive match")
        }
        var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
        _goml_runtime_core_string_println(inline214)
        var t184 Result__unit__string
        var inline203 bool = false
        var inline204 Result__unit__string = step(inline203)
        switch inline204.(type) {
        case Ok:
            var inline207 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t184 = inline207
            var t185 string
            switch t184.(type) {
            case Ok:
                t185 = "ok unit"
            case Err:
                var inline199 string = t184.(Err)._0
                var inline201 string = "err " + inline199
                t185 = inline201
            default:
                panic("non-exhaustive match")
            }
            var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
            _goml_runtime_core_string_println(inline195)
            return struct{}{}
        case Err:
            var inline210 string = inline204.(Err)._0
            var inline212 Result__unit__string = Err{
                _0: inline210,
            }
            t184 = inline212
            var t185 string
            switch t184.(type) {
            case Ok:
                t185 = "ok unit"
            case Err:
                var inline199 string = t184.(Err)._0
                var inline201 string = "err " + inline199
                t185 = inline201
            default:
                panic("non-exhaustive match")
            }
            var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
            _goml_runtime_core_string_println(inline195)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
