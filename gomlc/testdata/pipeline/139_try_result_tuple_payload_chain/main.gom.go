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

func split_host_port(ok__0 bool) _goml_m_Result_____o_string_c_string_q_____string {
    if ok__0 {
        var t425 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t426 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 0,
            _v0_0: t425,
        }
        return t426
    } else {
        var t427 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 1,
            _v1_0: "missing port",
        }
        return t427
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp411 _goml_m_Result_____o_string_c_string_q_____string
    var inline460 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp411 = inline460
    var jp434 Tuple2_6string_6string
    switch mtmp411._tag {
    case 0:
        var x412 Tuple2_6string_6string = mtmp411._v0_0
        jp434 = x412
        var x415 string = jp434._0
        var x416 string = jp434._1
        var t435 string = x415 + "="
        var t436 string = t435 + x416
        var t437 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t436,
        }
        return t437
    case 1:
        var x413 string = mtmp411._v1_0
        var t438 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t438
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t446 Result__string__string = render(true)
    var t447 string
    switch t446._tag {
    case 0:
        var inline475 string = t446._v0_0
        var inline477 string = "ok " + inline475
        t447 = inline477
    case 1:
        var inline478 string = t446._v1_0
        var inline480 string = "err " + inline478
        t447 = inline480
    default:
        panic("non-exhaustive match")
    }
    var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline472)
    var t448 Result__string__string = render(false)
    var t449 string
    switch t448._tag {
    case 0:
        var inline465 string = t448._v0_0
        var inline467 string = "ok " + inline465
        t449 = inline467
    case 1:
        var inline468 string = t448._v1_0
        var inline470 string = "err " + inline468
        t449 = inline470
    default:
        panic("non-exhaustive match")
    }
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline462)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
