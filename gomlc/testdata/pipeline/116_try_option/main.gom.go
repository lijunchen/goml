package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t417 Option__int32 = Some{
            _0: 4,
        }
        return t417
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t420 int32 = a__1 + b__2
    return t420
}

func main0() struct{} {
    var t434 Option__int32
    var inline479 bool = true
    var inline480 Option__int32 = maybe_value(inline479)
    var inline482 int32
    switch inline480.(type) {
    case None:
        t434 = None{}
        var t435 string
        switch t434.(type) {
        case None:
            t435 = "none"
        case Some:
            var inline474 int32 = t434.(Some)._0
            var inline476 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline474)
            var inline477 string = "some=" + inline476
            t435 = inline477
        default:
            panic("non-exhaustive match")
        }
        var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
        _goml_runtime_core_string_println(inline471)
        var t436 Option__int32
        var inline462 bool = false
        var inline463 Option__int32 = maybe_value(inline462)
        var inline465 int32
        switch inline463.(type) {
        case None:
            t436 = None{}
            var t437 string
            switch t436.(type) {
            case None:
                t437 = "none"
            case Some:
                var inline457 int32 = t436.(Some)._0
                var inline459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline457)
                var inline460 string = "some=" + inline459
                t437 = inline460
            default:
                panic("non-exhaustive match")
            }
            var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline454)
            return struct{}{}
        case Some:
            var inline468 int32 = inline463.(Some)._0
            inline465 = inline468
            var inline466 int32 = add(inline465, 2)
            var inline467 Option__int32 = Some{
                _0: inline466,
            }
            t436 = inline467
            var t437 string
            switch t436.(type) {
            case None:
                t437 = "none"
            case Some:
                var inline457 int32 = t436.(Some)._0
                var inline459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline457)
                var inline460 string = "some=" + inline459
                t437 = inline460
            default:
                panic("non-exhaustive match")
            }
            var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline454)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline485 int32 = inline480.(Some)._0
        inline482 = inline485
        var inline483 int32 = add(inline482, 2)
        var inline484 Option__int32 = Some{
            _0: inline483,
        }
        t434 = inline484
        var t435 string
        switch t434.(type) {
        case None:
            t435 = "none"
        case Some:
            var inline474 int32 = t434.(Some)._0
            var inline476 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline474)
            var inline477 string = "some=" + inline476
            t435 = inline477
        default:
            panic("non-exhaustive match")
        }
        var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
        _goml_runtime_core_string_println(inline471)
        var t436 Option__int32
        var inline462 bool = false
        var inline463 Option__int32 = maybe_value(inline462)
        var inline465 int32
        switch inline463.(type) {
        case None:
            t436 = None{}
            var t437 string
            switch t436.(type) {
            case None:
                t437 = "none"
            case Some:
                var inline457 int32 = t436.(Some)._0
                var inline459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline457)
                var inline460 string = "some=" + inline459
                t437 = inline460
            default:
                panic("non-exhaustive match")
            }
            var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline454)
            return struct{}{}
        case Some:
            var inline468 int32 = inline463.(Some)._0
            inline465 = inline468
            var inline466 int32 = add(inline465, 2)
            var inline467 Option__int32 = Some{
                _0: inline466,
            }
            t436 = inline467
            var t437 string
            switch t436.(type) {
            case None:
                t437 = "none"
            case Some:
                var inline457 int32 = t436.(Some)._0
                var inline459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline457)
                var inline460 string = "some=" + inline459
                t437 = inline460
            default:
                panic("non-exhaustive match")
            }
            var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline454)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t440 string = _goml_runtime_core_int32_to_string(self__33)
    return t440
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
