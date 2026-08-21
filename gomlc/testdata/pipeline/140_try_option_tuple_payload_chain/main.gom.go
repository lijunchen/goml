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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline451 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline452 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: inline451,
        }
        return inline452
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp411 _goml_m_Option_____o_string_c_string_q_
    var inline454 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp411 = inline454
    var jp431 Tuple2_6string_6string
    switch mtmp411._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x412 Tuple2_6string_6string = mtmp411._v1_0
        jp431 = x412
        var x414 string = jp431._0
        var x415 string = jp431._1
        var t432 string = x414 + ":"
        var t433 string = t432 + x415
        var t434 Option__string = Option__string{
            _tag: 1,
            _v1_0: t433,
        }
        return t434
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t441 Option__string = describe(true)
    var t442 string
    switch t441._tag {
    case 0:
        t442 = "none"
    case 1:
        var inline481 string = t441._v1_0
        var inline483 string = "some " + inline481
        t442 = inline483
    default:
        panic("non-exhaustive match")
    }
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t442)
    _goml_runtime_core_string_println(inline478)
    var t443 Option__string
    var inline463 bool = false
    var inline464 _goml_m_Option_____o_string_c_string_q_ = pair(inline463)
    var inline466 Tuple2_6string_6string
    switch inline464._tag {
    case 0:
        t443 = Option__string{
            _tag: 0,
        }
        var t444 string
        switch t443._tag {
        case 0:
            t444 = "none"
        case 1:
            var inline459 string = t443._v1_0
            var inline461 string = "some " + inline459
            t444 = inline461
        default:
            panic("non-exhaustive match")
        }
        var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
        _goml_runtime_core_string_println(inline456)
        return struct{}{}
    case 1:
        var inline475 Tuple2_6string_6string = inline464._v1_0
        inline466 = inline475
        var inline468 string = inline466._0
        var inline469 string = inline466._1
        var inline472 string = inline468 + ":"
        var inline473 string = inline472 + inline469
        var inline474 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline473,
        }
        t443 = inline474
        var t444 string
        switch t443._tag {
        case 0:
            t444 = "none"
        case 1:
            var inline459 string = t443._v1_0
            var inline461 string = "some " + inline459
            t444 = inline461
        default:
            panic("non-exhaustive match")
        }
        var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
        _goml_runtime_core_string_println(inline456)
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
