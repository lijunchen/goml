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

type _goml_m_Result_____o_string_c_string_q_____string struct {
    _tag int32
    _v0_0 Tuple2_6string_6string
    _v1_0 string
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func render(ok__1 bool) Result__string__string {
    var mtmp408 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline450 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline451 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 0,
            _v0_0: inline450,
        }
        mtmp408 = inline451
    } else {
        var inline452 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 1,
            _v1_0: "missing port",
        }
        mtmp408 = inline452
    }
    var jp428 Tuple2_6string_6string
    switch mtmp408._tag {
    case 0:
        var x409 Tuple2_6string_6string = mtmp408._v0_0
        jp428 = x409
        var x412 string = jp428._0
        var x413 string = jp428._1
        var t429 string = x412 + ":"
        var t430 string = t429 + x413
        var t431 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t430,
        }
        return t431
    case 1:
        var x410 string = mtmp408._v1_0
        var t432 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x410,
        }
        return t432
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t440 Result__string__string = render(true)
    var t441 string
    switch t440._tag {
    case 0:
        var inline467 string = t440._v0_0
        var inline469 string = "ok " + inline467
        t441 = inline469
    case 1:
        var inline470 string = t440._v1_0
        var inline472 string = "err " + inline470
        t441 = inline472
    default:
        panic("non-exhaustive match")
    }
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline464)
    var t442 Result__string__string = render(false)
    var t443 string
    switch t442._tag {
    case 0:
        var inline457 string = t442._v0_0
        var inline459 string = "ok " + inline457
        t443 = inline459
    case 1:
        var inline460 string = t442._v1_0
        var inline462 string = "err " + inline460
        t443 = inline462
    default:
        panic("non-exhaustive match")
    }
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline454)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
