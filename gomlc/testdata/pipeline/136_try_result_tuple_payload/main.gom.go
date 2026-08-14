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

type _goml_m_Result_____o_string_c_string_q_____string interface {
    is_goml_m_Result_____o_string_c_string_q_____string()
}

type _goml_m_Result_____o_string_c_string_q_____string_Ok struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Result_____o_string_c_string_q_____string_Ok) is_goml_m_Result_____o_string_c_string_q_____string() {}

type _goml_m_Result_____o_string_c_string_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_string_c_string_q_____string_Err) is_goml_m_Result_____o_string_c_string_q_____string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func render(ok__1 bool) Result__string__string {
    var mtmp408 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline450 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline451 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline450,
        }
        mtmp408 = inline451
    } else {
        var inline452 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp408 = inline452
    }
    var jp428 Tuple2_6string_6string
    switch mtmp408.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x409 Tuple2_6string_6string = mtmp408.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp428 = x409
        var x412 string = jp428._0
        var x413 string = jp428._1
        var t429 string = x412 + ":"
        var t430 string = t429 + x413
        var t431 Result__string__string = Result__string__string_Ok{
            _0: t430,
        }
        return t431
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x410 string = mtmp408.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t432 Result__string__string = Result__string__string_Err{
            _0: x410,
        }
        return t432
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t440 Result__string__string = render(true)
    var t441 string
    switch t440.(type) {
    case Result__string__string_Ok:
        var inline467 string = t440.(Result__string__string_Ok)._0
        var inline469 string = "ok " + inline467
        t441 = inline469
    case Result__string__string_Err:
        var inline470 string = t440.(Result__string__string_Err)._0
        var inline472 string = "err " + inline470
        t441 = inline472
    default:
        panic("non-exhaustive match")
    }
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline464)
    var t442 Result__string__string = render(false)
    var t443 string
    switch t442.(type) {
    case Result__string__string_Ok:
        var inline457 string = t442.(Result__string__string_Ok)._0
        var inline459 string = "ok " + inline457
        t443 = inline459
    case Result__string__string_Err:
        var inline460 string = t442.(Result__string__string_Err)._0
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
