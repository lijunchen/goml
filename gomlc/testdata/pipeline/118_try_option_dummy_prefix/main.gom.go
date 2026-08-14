package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func cut_prefix(case_id__0 int32) Option__string {
    var t417 bool = case_id__0 == 0
    if t417 {
        var t418 Option__string = Some{
            _0: "ml",
        }
        return t418
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t431 Option__string
    var inline468 int32 = 0
    var inline469 Option__string = cut_prefix(inline468)
    var inline471 string
    switch inline469.(type) {
    case None:
        t431 = None{}
        var t432 string
        switch t431.(type) {
        case None:
            t432 = "none"
        case Some:
            var inline464 string = t431.(Some)._0
            var inline466 string = "some " + inline464
            t432 = inline466
        default:
            panic("non-exhaustive match")
        }
        var inline461 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
        _goml_runtime_core_string_println(inline461)
        var t433 Option__string
        var inline451 int32 = 1
        var inline452 Option__string = cut_prefix(inline451)
        var inline454 string
        switch inline452.(type) {
        case None:
            t433 = None{}
            var t434 string
            switch t433.(type) {
            case None:
                t434 = "none"
            case Some:
                var inline447 string = t433.(Some)._0
                var inline449 string = "some " + inline447
                t434 = inline449
            default:
                panic("non-exhaustive match")
            }
            var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline444)
            return struct{}{}
        case Some:
            var inline458 string = inline452.(Some)._0
            inline454 = inline458
            var inline456 string = inline454 + "!"
            var inline457 Option__string = Some{
                _0: inline456,
            }
            t433 = inline457
            var t434 string
            switch t433.(type) {
            case None:
                t434 = "none"
            case Some:
                var inline447 string = t433.(Some)._0
                var inline449 string = "some " + inline447
                t434 = inline449
            default:
                panic("non-exhaustive match")
            }
            var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline444)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline475 string = inline469.(Some)._0
        inline471 = inline475
        var inline473 string = inline471 + "!"
        var inline474 Option__string = Some{
            _0: inline473,
        }
        t431 = inline474
        var t432 string
        switch t431.(type) {
        case None:
            t432 = "none"
        case Some:
            var inline464 string = t431.(Some)._0
            var inline466 string = "some " + inline464
            t432 = inline466
        default:
            panic("non-exhaustive match")
        }
        var inline461 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
        _goml_runtime_core_string_println(inline461)
        var t433 Option__string
        var inline451 int32 = 1
        var inline452 Option__string = cut_prefix(inline451)
        var inline454 string
        switch inline452.(type) {
        case None:
            t433 = None{}
            var t434 string
            switch t433.(type) {
            case None:
                t434 = "none"
            case Some:
                var inline447 string = t433.(Some)._0
                var inline449 string = "some " + inline447
                t434 = inline449
            default:
                panic("non-exhaustive match")
            }
            var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline444)
            return struct{}{}
        case Some:
            var inline458 string = inline452.(Some)._0
            inline454 = inline458
            var inline456 string = inline454 + "!"
            var inline457 Option__string = Some{
                _0: inline456,
            }
            t433 = inline457
            var t434 string
            switch t433.(type) {
            case None:
                t434 = "none"
            case Some:
                var inline447 string = t433.(Some)._0
                var inline449 string = "some " + inline447
                t434 = inline449
            default:
                panic("non-exhaustive match")
            }
            var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline444)
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
