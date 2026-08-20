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
        var t422 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t423 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 0,
            _v0_0: t422,
        }
        return t423
    } else {
        var t424 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 1,
            _v1_0: "missing port",
        }
        return t424
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp408 _goml_m_Result_____o_string_c_string_q_____string
    var inline457 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp408 = inline457
    var jp431 Tuple2_6string_6string
    switch mtmp408._tag {
    case 0:
        var x409 Tuple2_6string_6string = mtmp408._v0_0
        jp431 = x409
        var x412 string = jp431._0
        var x413 string = jp431._1
        var t432 string = x412 + "="
        var t433 string = t432 + x413
        var t434 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t433,
        }
        return t434
    case 1:
        var x410 string = mtmp408._v1_0
        var t435 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x410,
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
        var inline472 string = t443._v0_0
        var inline474 string = "ok " + inline472
        t444 = inline474
    case 1:
        var inline475 string = t443._v1_0
        var inline477 string = "err " + inline475
        t444 = inline477
    default:
        panic("non-exhaustive match")
    }
    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
    _goml_runtime_core_string_println(inline469)
    var t445 Result__string__string = render(false)
    var t446 string
    switch t445._tag {
    case 0:
        var inline462 string = t445._v0_0
        var inline464 string = "ok " + inline462
        t446 = inline464
    case 1:
        var inline465 string = t445._v1_0
        var inline467 string = "err " + inline465
        t446 = inline467
    default:
        panic("non-exhaustive match")
    }
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline459)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
