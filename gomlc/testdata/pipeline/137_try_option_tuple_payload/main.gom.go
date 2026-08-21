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
        var t423 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t424 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: t423,
        }
        return t424
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp411 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline448 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline449 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: inline448,
        }
        mtmp411 = inline449
    } else {
        mtmp411 = _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
    var jp428 Tuple2_6string_6string
    switch mtmp411._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x412 Tuple2_6string_6string = mtmp411._v1_0
        jp428 = x412
        var x414 string = jp428._0
        var x415 string = jp428._1
        var t429 string = x414 + "|"
        var t430 string = t429 + x415
        var t431 Option__string = Option__string{
            _tag: 1,
            _v1_0: t430,
        }
        return t431
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t438 Option__string = describe(true)
    var t439 string
    switch t438._tag {
    case 0:
        t439 = "none"
    case 1:
        var inline476 string = t438._v1_0
        var inline478 string = "some " + inline476
        t439 = inline478
    default:
        panic("non-exhaustive match")
    }
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline473)
    var t440 Option__string
    var inline458 bool = false
    var inline459 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline458)
    var inline461 Tuple2_6string_6string
    switch inline459._tag {
    case 0:
        t440 = Option__string{
            _tag: 0,
        }
        var t441 string
        switch t440._tag {
        case 0:
            t441 = "none"
        case 1:
            var inline454 string = t440._v1_0
            var inline456 string = "some " + inline454
            t441 = inline456
        default:
            panic("non-exhaustive match")
        }
        var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline451)
        return struct{}{}
    case 1:
        var inline470 Tuple2_6string_6string = inline459._v1_0
        inline461 = inline470
        var inline463 string = inline461._0
        var inline464 string = inline461._1
        var inline467 string = inline463 + "|"
        var inline468 string = inline467 + inline464
        var inline469 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline468,
        }
        t440 = inline469
        var t441 string
        switch t440._tag {
        case 0:
            t441 = "none"
        case 1:
            var inline454 string = t440._v1_0
            var inline456 string = "some " + inline454
            t441 = inline456
        default:
            panic("non-exhaustive match")
        }
        var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline451)
        return struct{}{}
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
