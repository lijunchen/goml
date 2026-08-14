package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_float32_to_string(x float32) string {
    var formatted string = _goml_strconv.FormatFloat(float64(x), 102, -1, 32)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_float64_to_string(x float64) string {
    var formatted string = _goml_strconv.FormatFloat(x, 102, -1, 64)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_7float32_7float64 struct {
    _0 float32
    _1 float64
}

type Ordering int32

type FloatEvent interface {
    isFloatEvent()
}

type Sample32 struct {
    _0 string
    _1 float32
}

func (_ Sample32) isFloatEvent() {}

type Sample64 struct {
    _0 string
    _1 float64
}

func (_ Sample64) isFloatEvent() {}

func summarize(event__0 FloatEvent) string {
    switch event__0.(type) {
    case Sample32:
        var x408 string = event__0.(Sample32)._0
        var x409 float32 = event__0.(Sample32)._1
        var t419 string
        var inline454 string = _goml_runtime_core_float32_to_string(x409)
        t419 = inline454
        var t420 string = x408 + t419
        return t420
    case Sample64:
        var x410 string = event__0.(Sample64)._0
        var x411 float64 = event__0.(Sample64)._1
        var t421 string
        var inline456 string = _goml_runtime_core_float64_to_string(x411)
        t421 = inline456
        var t422 string = x410 + t421
        return t422
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var first_value__12 float32 = 0.5
    var second_value__13 float32 = 2.25
    var third_value__14 float64 = 9.5
    var first__15 FloatEvent = Sample32{
        _0: "f32=",
        _1: first_value__12,
    }
    var second__16 FloatEvent = Sample32{
        _0: "f32_b=",
        _1: second_value__13,
    }
    var t431 string = summarize(first__15)
    var t432 string = summarize(second__16)
    var t433 string = t431 + t432
    var t434 string
    var inline499 string = "f64="
    var inline503 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline504 string = inline499 + inline503
    t434 = inline504
    var t435 string = t433 + t434
    var t436 string
    var inline479 float32 = 0.75
    var inline480 float64 = 4
    var inline483 float32 = 1
    var inline484 float64 = 5
    var inline485 bool = inline479 < inline483
    var inline486 bool = inline480 < inline484
    var inline487 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline485)
    var inline488 string = "left<1?=" + inline487
    var inline489 string = inline488 + ",right<5?="
    var inline490 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline486)
    var inline491 string = inline489 + inline490
    t436 = inline491
    var t437 string = t435 + t436
    var t438 string
    var inline465 float32 = 1.5
    var inline466 float64 = 7.25
    var inline469 float32 = 1
    var inline470 float64 = 5
    var inline471 bool = inline465 < inline469
    var inline472 bool = inline466 < inline470
    var inline473 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline471)
    var inline474 string = "left<1?=" + inline473
    var inline475 string = inline474 + ",right<5?="
    var inline476 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline472)
    var inline477 string = inline475 + inline476
    t438 = inline477
    var message__20 string = t437 + t438
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline462)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__161 float64) string {
    var t444 string = _goml_runtime_core_float64_to_string(self__161)
    return t444
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t447 string = _goml_runtime_core_bool_to_string(self__148)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
