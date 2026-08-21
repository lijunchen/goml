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

type Option__int32 struct {
    _tag int32
    _v0_0 int32
}

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t419 Option__int32 = Option__int32{
            _tag: 0,
            _v0_0: 41,
        }
        return t419
    } else {
        return Option__int32{
            _tag: 1,
        }
    }
}

func main0() struct{} {
    var t432 Option__int32
    var inline474 bool = true
    var inline475 Option__int32 = maybe_value(inline474)
    var inline477 int32
    switch inline475._tag {
    case 0:
        var inline481 int32 = inline475._v0_0
        inline477 = inline481
        var inline479 int32 = inline477 + 1
        var inline480 Option__int32 = Option__int32{
            _tag: 0,
            _v0_0: inline479,
        }
        t432 = inline480
        var t433 string
        switch t432._tag {
        case 0:
            var inline470 int32 = t432._v0_0
            var inline472 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline470)
            t433 = inline472
        case 1:
            t433 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
        _goml_runtime_core_string_println(inline467)
        var t434 Option__int32
        var inline457 bool = false
        var inline458 Option__int32 = maybe_value(inline457)
        var inline460 int32
        switch inline458._tag {
        case 0:
            var inline464 int32 = inline458._v0_0
            inline460 = inline464
            var inline462 int32 = inline460 + 1
            var inline463 Option__int32 = Option__int32{
                _tag: 0,
                _v0_0: inline462,
            }
            t434 = inline463
            var t435 string
            switch t434._tag {
            case 0:
                var inline453 int32 = t434._v0_0
                var inline455 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline453)
                t435 = inline455
            case 1:
                t435 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline450)
            return struct{}{}
        case 1:
            t434 = Option__int32{
                _tag: 1,
            }
            var t435 string
            switch t434._tag {
            case 0:
                var inline453 int32 = t434._v0_0
                var inline455 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline453)
                t435 = inline455
            case 1:
                t435 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline450)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        t432 = Option__int32{
            _tag: 1,
        }
        var t433 string
        switch t432._tag {
        case 0:
            var inline470 int32 = t432._v0_0
            var inline472 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline470)
            t433 = inline472
        case 1:
            t433 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
        _goml_runtime_core_string_println(inline467)
        var t434 Option__int32
        var inline457 bool = false
        var inline458 Option__int32 = maybe_value(inline457)
        var inline460 int32
        switch inline458._tag {
        case 0:
            var inline464 int32 = inline458._v0_0
            inline460 = inline464
            var inline462 int32 = inline460 + 1
            var inline463 Option__int32 = Option__int32{
                _tag: 0,
                _v0_0: inline462,
            }
            t434 = inline463
            var t435 string
            switch t434._tag {
            case 0:
                var inline453 int32 = t434._v0_0
                var inline455 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline453)
                t435 = inline455
            case 1:
                t435 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline450)
            return struct{}{}
        case 1:
            t434 = Option__int32{
                _tag: 1,
            }
            var t435 string
            switch t434._tag {
            case 0:
                var inline453 int32 = t434._v0_0
                var inline455 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline453)
                t435 = inline455
            case 1:
                t435 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline450)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t439 string = _goml_runtime_core_int32_to_string(self__33)
    return t439
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
