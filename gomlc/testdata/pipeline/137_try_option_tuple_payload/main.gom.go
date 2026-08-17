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

type Option__string struct {
    _tag int32
    _v1_0 string
}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__0 {
        var t420 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t421 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t420,
        }
        return t421
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp408 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline445 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline446 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline445,
        }
        mtmp408 = inline446
    } else {
        mtmp408 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp425 Tuple2_6string_6string
    switch mtmp408.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string{
            _tag: 0,
        }
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x409 Tuple2_6string_6string = mtmp408.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp425 = x409
        var x411 string = jp425._0
        var x412 string = jp425._1
        var t426 string = x411 + "|"
        var t427 string = t426 + x412
        var t428 Option__string = Option__string{
            _tag: 1,
            _v1_0: t427,
        }
        return t428
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t435 Option__string = describe(true)
    var t436 string
    switch t435._tag {
    case 0:
        t436 = "none"
    case 1:
        var inline473 string = t435._v1_0
        var inline475 string = "some " + inline473
        t436 = inline475
    default:
        panic("non-exhaustive match")
    }
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline470)
    var t437 Option__string
    var inline455 bool = false
    var inline456 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline455)
    var inline458 Tuple2_6string_6string
    switch inline456.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t437 = Option__string{
            _tag: 0,
        }
        var t438 string
        switch t437._tag {
        case 0:
            t438 = "none"
        case 1:
            var inline451 string = t437._v1_0
            var inline453 string = "some " + inline451
            t438 = inline453
        default:
            panic("non-exhaustive match")
        }
        var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
        _goml_runtime_core_string_println(inline448)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline467 Tuple2_6string_6string = inline456.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline458 = inline467
        var inline460 string = inline458._0
        var inline461 string = inline458._1
        var inline464 string = inline460 + "|"
        var inline465 string = inline464 + inline461
        var inline466 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline465,
        }
        t437 = inline466
        var t438 string
        switch t437._tag {
        case 0:
            t438 = "none"
        case 1:
            var inline451 string = t437._v1_0
            var inline453 string = "some " + inline451
            t438 = inline453
        default:
            panic("non-exhaustive match")
        }
        var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
        _goml_runtime_core_string_println(inline448)
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
