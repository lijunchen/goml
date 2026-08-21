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
        var t421 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t422 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: t421,
        }
        return t422
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func main0() struct{} {
    var t434 Option__string
    var inline470 bool = true
    var inline471 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline470)
    switch inline471._tag {
    case 0:
        t434 = Option__string{
            _tag: 0,
        }
        var t435 string
        switch t434._tag {
        case 0:
            t435 = "none"
        case 1:
            var inline466 string = t434._v1_0
            var inline468 string = "some " + inline466
            t435 = inline468
        default:
            panic("non-exhaustive match")
        }
        var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
        _goml_runtime_core_string_println(inline463)
        var t436 Option__string
        var inline454 bool = false
        var inline455 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline454)
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
            var inline459 Option__string = Option__string{
                _tag: 1,
                _v1_0: "ok",
            }
            t436 = inline459
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
        var inline475 Option__string = Option__string{
            _tag: 1,
            _v1_0: "ok",
        }
        t434 = inline475
        var t435 string
        switch t434._tag {
        case 0:
            t435 = "none"
        case 1:
            var inline466 string = t434._v1_0
            var inline468 string = "some " + inline466
            t435 = inline468
        default:
            panic("non-exhaustive match")
        }
        var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
        _goml_runtime_core_string_println(inline463)
        var t436 Option__string
        var inline454 bool = false
        var inline455 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline454)
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
            var inline459 Option__string = Option__string{
                _tag: 1,
                _v1_0: "ok",
            }
            t436 = inline459
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
