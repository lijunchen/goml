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
        var t148 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t148
    } else {
        var t149 Result__unit__string = Err{
            _0: "step failed",
        }
        return t149
    }
}

func main0() struct{} {
    var t163 Result__unit__string
    var inline203 bool = true
    var inline204 Result__unit__string = step(inline203)
    switch inline204.(type) {
    case Ok:
        var inline207 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t163 = inline207
        var t164 string
        switch t163.(type) {
        case Ok:
            t164 = "ok unit"
        case Err:
            var inline199 string = t163.(Err)._0
            var inline201 string = "err " + inline199
            t164 = inline201
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
        _goml_runtime_core_string_println(inline195)
        var t165 Result__unit__string
        var inline184 bool = false
        var inline185 Result__unit__string = step(inline184)
        switch inline185.(type) {
        case Ok:
            var inline188 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t165 = inline188
            var t166 string
            switch t165.(type) {
            case Ok:
                t166 = "ok unit"
            case Err:
                var inline180 string = t165.(Err)._0
                var inline182 string = "err " + inline180
                t166 = inline182
            default:
                panic("non-exhaustive match")
            }
            var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
            _goml_runtime_core_string_println(inline176)
            return struct{}{}
        case Err:
            var inline191 string = inline185.(Err)._0
            var inline193 Result__unit__string = Err{
                _0: inline191,
            }
            t165 = inline193
            var t166 string
            switch t165.(type) {
            case Ok:
                t166 = "ok unit"
            case Err:
                var inline180 string = t165.(Err)._0
                var inline182 string = "err " + inline180
                t166 = inline182
            default:
                panic("non-exhaustive match")
            }
            var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
            _goml_runtime_core_string_println(inline176)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline210 string = inline204.(Err)._0
        var inline212 Result__unit__string = Err{
            _0: inline210,
        }
        t163 = inline212
        var t164 string
        switch t163.(type) {
        case Ok:
            t164 = "ok unit"
        case Err:
            var inline199 string = t163.(Err)._0
            var inline201 string = "err " + inline199
            t164 = inline201
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
        _goml_runtime_core_string_println(inline195)
        var t165 Result__unit__string
        var inline184 bool = false
        var inline185 Result__unit__string = step(inline184)
        switch inline185.(type) {
        case Ok:
            var inline188 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t165 = inline188
            var t166 string
            switch t165.(type) {
            case Ok:
                t166 = "ok unit"
            case Err:
                var inline180 string = t165.(Err)._0
                var inline182 string = "err " + inline180
                t166 = inline182
            default:
                panic("non-exhaustive match")
            }
            var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
            _goml_runtime_core_string_println(inline176)
            return struct{}{}
        case Err:
            var inline191 string = inline185.(Err)._0
            var inline193 Result__unit__string = Err{
                _0: inline191,
            }
            t165 = inline193
            var t166 string
            switch t165.(type) {
            case Ok:
                t166 = "ok unit"
            case Err:
                var inline180 string = t165.(Err)._0
                var inline182 string = "err " + inline180
                t166 = inline182
            default:
                panic("non-exhaustive match")
            }
            var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
            _goml_runtime_core_string_println(inline176)
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
