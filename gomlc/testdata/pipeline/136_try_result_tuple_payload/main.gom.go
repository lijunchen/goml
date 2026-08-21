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
    var mtmp411 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline453 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline454 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 0,
            _v0_0: inline453,
        }
        mtmp411 = inline454
    } else {
        var inline455 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 1,
            _v1_0: "missing port",
        }
        mtmp411 = inline455
    }
    var jp431 Tuple2_6string_6string
    switch mtmp411._tag {
    case 0:
        var x412 Tuple2_6string_6string = mtmp411._v0_0
        jp431 = x412
        var x415 string = jp431._0
        var x416 string = jp431._1
        var t432 string = x415 + ":"
        var t433 string = t432 + x416
        var t434 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t433,
        }
        return t434
    case 1:
        var x413 string = mtmp411._v1_0
        var t435 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t435
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t443 Result__string__string = render(true)
    var t444 string
    switch t443._tag {
    case 0:
        var inline470 string = t443._v0_0
        var inline472 string = "ok " + inline470
        t444 = inline472
    case 1:
        var inline473 string = t443._v1_0
        var inline475 string = "err " + inline473
        t444 = inline475
    default:
        panic("non-exhaustive match")
    }
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
    _goml_runtime_core_string_println(inline467)
    var t445 Result__string__string = render(false)
    var t446 string
    switch t445._tag {
    case 0:
        var inline460 string = t445._v0_0
        var inline462 string = "ok " + inline460
        t446 = inline462
    case 1:
        var inline463 string = t445._v1_0
        var inline465 string = "err " + inline463
        t446 = inline465
    default:
        panic("non-exhaustive match")
    }
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
