package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

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
        var t420 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t420
    } else {
        var t421 Result__unit__string = Err{
            _0: "step failed",
        }
        return t421
    }
}

func main0() struct{} {
    var t435 Result__unit__string
    var inline475 bool = true
    var inline476 Result__unit__string = step(inline475)
    switch inline476.(type) {
    case Ok:
        var inline479 Result__unit__string = Ok{
            _0: struct{}{},
        }
        t435 = inline479
        var t436 string
        switch t435.(type) {
        case Ok:
            t436 = "ok unit"
        case Err:
            var inline471 string = t435.(Err)._0
            var inline473 string = "err " + inline471
            t436 = inline473
        default:
            panic("non-exhaustive match")
        }
        var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
        _goml_runtime_core_string_println(inline467)
        var t437 Result__unit__string
        var inline456 bool = false
        var inline457 Result__unit__string = step(inline456)
        switch inline457.(type) {
        case Ok:
            var inline460 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t437 = inline460
            var t438 string
            switch t437.(type) {
            case Ok:
                t438 = "ok unit"
            case Err:
                var inline452 string = t437.(Err)._0
                var inline454 string = "err " + inline452
                t438 = inline454
            default:
                panic("non-exhaustive match")
            }
            var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
            _goml_runtime_core_string_println(inline448)
            return struct{}{}
        case Err:
            var inline463 string = inline457.(Err)._0
            var inline465 Result__unit__string = Err{
                _0: inline463,
            }
            t437 = inline465
            var t438 string
            switch t437.(type) {
            case Ok:
                t438 = "ok unit"
            case Err:
                var inline452 string = t437.(Err)._0
                var inline454 string = "err " + inline452
                t438 = inline454
            default:
                panic("non-exhaustive match")
            }
            var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
            _goml_runtime_core_string_println(inline448)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline482 string = inline476.(Err)._0
        var inline484 Result__unit__string = Err{
            _0: inline482,
        }
        t435 = inline484
        var t436 string
        switch t435.(type) {
        case Ok:
            t436 = "ok unit"
        case Err:
            var inline471 string = t435.(Err)._0
            var inline473 string = "err " + inline471
            t436 = inline473
        default:
            panic("non-exhaustive match")
        }
        var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
        _goml_runtime_core_string_println(inline467)
        var t437 Result__unit__string
        var inline456 bool = false
        var inline457 Result__unit__string = step(inline456)
        switch inline457.(type) {
        case Ok:
            var inline460 Result__unit__string = Ok{
                _0: struct{}{},
            }
            t437 = inline460
            var t438 string
            switch t437.(type) {
            case Ok:
                t438 = "ok unit"
            case Err:
                var inline452 string = t437.(Err)._0
                var inline454 string = "err " + inline452
                t438 = inline454
            default:
                panic("non-exhaustive match")
            }
            var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
            _goml_runtime_core_string_println(inline448)
            return struct{}{}
        case Err:
            var inline463 string = inline457.(Err)._0
            var inline465 Result__unit__string = Err{
                _0: inline463,
            }
            t437 = inline465
            var t438 string
            switch t437.(type) {
            case Ok:
                t438 = "ok unit"
            case Err:
                var inline452 string = t437.(Err)._0
                var inline454 string = "err " + inline452
                t438 = inline454
            default:
                panic("non-exhaustive match")
            }
            var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
            _goml_runtime_core_string_println(inline448)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
