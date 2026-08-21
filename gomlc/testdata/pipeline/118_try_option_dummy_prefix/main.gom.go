package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Option__string struct {
    _tag int32
    _v1_0 string
}

func cut_prefix(case_id__0 int32) Option__string {
    var t420 bool = case_id__0 == 0
    if t420 {
        var t421 Option__string = Option__string{
            _tag: 1,
            _v1_0: "ml",
        }
        return t421
    } else {
        return Option__string{
            _tag: 0,
        }
    }
}

func main0() struct{} {
    var t434 Option__string
    var inline471 int32 = 0
    var inline472 Option__string = cut_prefix(inline471)
    var inline474 string
    switch inline472._tag {
    case 0:
        t434 = Option__string{
            _tag: 0,
        }
        var t435 string
        switch t434._tag {
        case 0:
            t435 = "none"
        case 1:
            var inline467 string = t434._v1_0
            var inline469 string = "some " + inline467
            t435 = inline469
        default:
            panic("non-exhaustive match")
        }
        var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
        _goml_runtime_core_string_println(inline464)
        var t436 Option__string
        var inline454 int32 = 1
        var inline455 Option__string = cut_prefix(inline454)
        var inline457 string
        switch inline455._tag {
        case 0:
            t436 = Option__string{
                _tag: 0,
            }
            var t437 string
            switch t436._tag {
            case 0:
                t437 = "none"
            case 1:
                var inline450 string = t436._v1_0
                var inline452 string = "some " + inline450
                t437 = inline452
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        case 1:
            var inline461 string = inline455._v1_0
            inline457 = inline461
            var inline459 string = inline457 + "!"
            var inline460 Option__string = Option__string{
                _tag: 1,
                _v1_0: inline459,
            }
            t436 = inline460
            var t437 string
            switch t436._tag {
            case 0:
                t437 = "none"
            case 1:
                var inline450 string = t436._v1_0
                var inline452 string = "some " + inline450
                t437 = inline452
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline478 string = inline472._v1_0
        inline474 = inline478
        var inline476 string = inline474 + "!"
        var inline477 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline476,
        }
        t434 = inline477
        var t435 string
        switch t434._tag {
        case 0:
            t435 = "none"
        case 1:
            var inline467 string = t434._v1_0
            var inline469 string = "some " + inline467
            t435 = inline469
        default:
            panic("non-exhaustive match")
        }
        var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
        _goml_runtime_core_string_println(inline464)
        var t436 Option__string
        var inline454 int32 = 1
        var inline455 Option__string = cut_prefix(inline454)
        var inline457 string
        switch inline455._tag {
        case 0:
            t436 = Option__string{
                _tag: 0,
            }
            var t437 string
            switch t436._tag {
            case 0:
                t437 = "none"
            case 1:
                var inline450 string = t436._v1_0
                var inline452 string = "some " + inline450
                t437 = inline452
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        case 1:
            var inline461 string = inline455._v1_0
            inline457 = inline461
            var inline459 string = inline457 + "!"
            var inline460 Option__string = Option__string{
                _tag: 1,
                _v1_0: inline459,
            }
            t436 = inline460
            var t437 string
            switch t436._tag {
            case 0:
                t437 = "none"
            case 1:
                var inline450 string = t436._v1_0
                var inline452 string = "some " + inline450
                t437 = inline452
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline447)
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
