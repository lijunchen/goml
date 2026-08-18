package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Ordering int32

type _goml_m_Option_____o_string_c_string_q_ struct {
    _tag int32
    _v1_0 Tuple2_6string_6string
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__0 {
        var t418 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t419 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: t418,
        }
        return t419
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func main0() struct{} {
    var t431 Option__string
    var inline467 bool = true
    var inline468 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline467)
    switch inline468._tag {
    case 0:
        t431 = Option__string{
            _tag: 0,
        }
        var t432 string
        switch t431._tag {
        case 0:
            t432 = "none"
        case 1:
            var inline463 string = t431._v1_0
            var inline465 string = "some " + inline463
            t432 = inline465
        default:
            panic("non-exhaustive match")
        }
        var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
        _goml_runtime_core_string_println(inline460)
        var t433 Option__string
        var inline451 bool = false
        var inline452 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline451)
        switch inline452._tag {
        case 0:
            t433 = Option__string{
                _tag: 0,
            }
            var t434 string
            switch t433._tag {
            case 0:
                t434 = "none"
            case 1:
                var inline447 string = t433._v1_0
                var inline449 string = "some " + inline447
                t434 = inline449
            default:
                panic("non-exhaustive match")
            }
            var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline444)
            return struct{}{}
        case 1:
            var inline456 Option__string = Option__string{
                _tag: 1,
                _v1_0: "ok",
            }
            t433 = inline456
            var t434 string
            switch t433._tag {
            case 0:
                t434 = "none"
            case 1:
                var inline447 string = t433._v1_0
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
    case 1:
        var inline472 Option__string = Option__string{
            _tag: 1,
            _v1_0: "ok",
        }
        t431 = inline472
        var t432 string
        switch t431._tag {
        case 0:
            t432 = "none"
        case 1:
            var inline463 string = t431._v1_0
            var inline465 string = "some " + inline463
            t432 = inline465
        default:
            panic("non-exhaustive match")
        }
        var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
        _goml_runtime_core_string_println(inline460)
        var t433 Option__string
        var inline451 bool = false
        var inline452 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline451)
        switch inline452._tag {
        case 0:
            t433 = Option__string{
                _tag: 0,
            }
            var t434 string
            switch t433._tag {
            case 0:
                t434 = "none"
            case 1:
                var inline447 string = t433._v1_0
                var inline449 string = "some " + inline447
                t434 = inline449
            default:
                panic("non-exhaustive match")
            }
            var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline444)
            return struct{}{}
        case 1:
            var inline456 Option__string = Option__string{
                _tag: 1,
                _v1_0: "ok",
            }
            t433 = inline456
            var t434 string
            switch t433._tag {
            case 0:
                t434 = "none"
            case 1:
                var inline447 string = t433._v1_0
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
