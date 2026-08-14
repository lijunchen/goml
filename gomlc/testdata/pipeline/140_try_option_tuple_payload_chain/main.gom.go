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

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__0 {
        var t420 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t421 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t420,
        }
        return t421
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline448 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline449 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline448,
        }
        return inline449
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp408 _goml_m_Option_____o_string_c_string_q_
    var inline451 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp408 = inline451
    var jp428 Tuple2_6string_6string
    switch mtmp408.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x409 Tuple2_6string_6string = mtmp408.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp428 = x409
        var x411 string = jp428._0
        var x412 string = jp428._1
        var t429 string = x411 + ":"
        var t430 string = t429 + x412
        var t431 Option__string = Option__string_Some{
            _0: t430,
        }
        return t431
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t438 Option__string = describe(true)
    var t439 string
    switch t438.(type) {
    case Option__string_None:
        t439 = "none"
    case Option__string_Some:
        var inline478 string = t438.(Option__string_Some)._0
        var inline480 string = "some " + inline478
        t439 = inline480
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline475)
    var t440 Option__string
    var inline460 bool = false
    var inline461 _goml_m_Option_____o_string_c_string_q_ = pair(inline460)
    var inline463 Tuple2_6string_6string
    switch inline461.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t440 = Option__string_None{}
        var t441 string
        switch t440.(type) {
        case Option__string_None:
            t441 = "none"
        case Option__string_Some:
            var inline456 string = t440.(Option__string_Some)._0
            var inline458 string = "some " + inline456
            t441 = inline458
        default:
            panic("non-exhaustive match")
        }
        var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline453)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline472 Tuple2_6string_6string = inline461.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline463 = inline472
        var inline465 string = inline463._0
        var inline466 string = inline463._1
        var inline469 string = inline465 + ":"
        var inline470 string = inline469 + inline466
        var inline471 Option__string = Option__string_Some{
            _0: inline470,
        }
        t440 = inline471
        var t441 string
        switch t440.(type) {
        case Option__string_None:
            t441 = "none"
        case Option__string_Some:
            var inline456 string = t440.(Option__string_Some)._0
            var inline458 string = "some " + inline456
            t441 = inline458
        default:
            panic("non-exhaustive match")
        }
        var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline453)
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
