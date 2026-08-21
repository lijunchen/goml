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
        var t423 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t423
    } else {
        var t424 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "step failed",
        }
        return t424
    }
}

func main0() struct{} {
    var t438 Result__unit__string
    var inline478 bool = true
    var inline479 Result__unit__string = step(inline478)
    switch inline479._tag {
    case 0:
        var inline482 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        t438 = inline482
        var t439 string
        switch t438._tag {
        case 0:
            t439 = "ok unit"
        case 1:
            var inline474 string = t438._v1_0
            var inline476 string = "err " + inline474
            t439 = inline476
        default:
            panic("non-exhaustive match")
        }
        var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
        _goml_runtime_core_string_println(inline470)
        var t440 Result__unit__string
        var inline459 bool = false
        var inline460 Result__unit__string = step(inline459)
        switch inline460._tag {
        case 0:
            var inline463 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t440 = inline463
            var t441 string
            switch t440._tag {
            case 0:
                t441 = "ok unit"
            case 1:
                var inline455 string = t440._v1_0
                var inline457 string = "err " + inline455
                t441 = inline457
            default:
                panic("non-exhaustive match")
            }
            var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
            _goml_runtime_core_string_println(inline451)
            return struct{}{}
        case 1:
            var inline466 string = inline460._v1_0
            var inline468 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline466,
            }
            t440 = inline468
            var t441 string
            switch t440._tag {
            case 0:
                t441 = "ok unit"
            case 1:
                var inline455 string = t440._v1_0
                var inline457 string = "err " + inline455
                t441 = inline457
            default:
                panic("non-exhaustive match")
            }
            var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
            _goml_runtime_core_string_println(inline451)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline485 string = inline479._v1_0
        var inline487 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: inline485,
        }
        t438 = inline487
        var t439 string
        switch t438._tag {
        case 0:
            t439 = "ok unit"
        case 1:
            var inline474 string = t438._v1_0
            var inline476 string = "err " + inline474
            t439 = inline476
        default:
            panic("non-exhaustive match")
        }
        var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
        _goml_runtime_core_string_println(inline470)
        var t440 Result__unit__string
        var inline459 bool = false
        var inline460 Result__unit__string = step(inline459)
        switch inline460._tag {
        case 0:
            var inline463 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t440 = inline463
            var t441 string
            switch t440._tag {
            case 0:
                t441 = "ok unit"
            case 1:
                var inline455 string = t440._v1_0
                var inline457 string = "err " + inline455
                t441 = inline457
            default:
                panic("non-exhaustive match")
            }
            var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
            _goml_runtime_core_string_println(inline451)
            return struct{}{}
        case 1:
            var inline466 string = inline460._v1_0
            var inline468 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline466,
            }
            t440 = inline468
            var t441 string
            switch t440._tag {
            case 0:
                t441 = "ok unit"
            case 1:
                var inline455 string = t440._v1_0
                var inline457 string = "err " + inline455
                t441 = inline457
            default:
                panic("non-exhaustive match")
            }
            var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
            _goml_runtime_core_string_println(inline451)
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
