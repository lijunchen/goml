package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

func step(ok__0 bool) Result__unit__string {
    if ok__0 {
        var t420 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t420
    } else {
        var t421 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "step failed",
        }
        return t421
    }
}

func main0() struct{} {
    var t435 Result__unit__string
    var inline475 bool = true
    var inline476 Result__unit__string = step(inline475)
    switch inline476._tag {
    case 0:
        var inline479 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        t435 = inline479
        var t436 string
        switch t435._tag {
        case 0:
            t436 = "ok unit"
        case 1:
            var inline471 string = t435._v1_0
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
        switch inline457._tag {
        case 0:
            var inline460 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t437 = inline460
            var t438 string
            switch t437._tag {
            case 0:
                t438 = "ok unit"
            case 1:
                var inline452 string = t437._v1_0
                var inline454 string = "err " + inline452
                t438 = inline454
            default:
                panic("non-exhaustive match")
            }
            var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
            _goml_runtime_core_string_println(inline448)
            return struct{}{}
        case 1:
            var inline463 string = inline457._v1_0
            var inline465 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline463,
            }
            t437 = inline465
            var t438 string
            switch t437._tag {
            case 0:
                t438 = "ok unit"
            case 1:
                var inline452 string = t437._v1_0
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
    case 1:
        var inline482 string = inline476._v1_0
        var inline484 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: inline482,
        }
        t435 = inline484
        var t436 string
        switch t435._tag {
        case 0:
            t436 = "ok unit"
        case 1:
            var inline471 string = t435._v1_0
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
        switch inline457._tag {
        case 0:
            var inline460 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t437 = inline460
            var t438 string
            switch t437._tag {
            case 0:
                t438 = "ok unit"
            case 1:
                var inline452 string = t437._v1_0
                var inline454 string = "err " + inline452
                t438 = inline454
            default:
                panic("non-exhaustive match")
            }
            var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
            _goml_runtime_core_string_println(inline448)
            return struct{}{}
        case 1:
            var inline463 string = inline457._v1_0
            var inline465 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline463,
            }
            t437 = inline465
            var t438 string
            switch t437._tag {
            case 0:
                t438 = "ok unit"
            case 1:
                var inline452 string = t437._v1_0
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
