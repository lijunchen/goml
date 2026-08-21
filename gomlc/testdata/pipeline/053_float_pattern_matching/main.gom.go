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
        var x411 string = event__0.(Sample32)._0
        var x412 float32 = event__0.(Sample32)._1
        var t422 string
        var inline457 string = _goml_runtime_core_float32_to_string(x412)
        t422 = inline457
        var t423 string = x411 + t422
        return t423
    case Sample64:
        var x413 string = event__0.(Sample64)._0
        var x414 float64 = event__0.(Sample64)._1
        var t424 string
        var inline459 string = _goml_runtime_core_float64_to_string(x414)
        t424 = inline459
        var t425 string = x413 + t424
        return t425
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
    var t434 string = summarize(first__15)
    var t435 string = summarize(second__16)
    var t436 string = t434 + t435
    var t437 string
    var inline502 string = "f64="
    var inline506 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline507 string = inline502 + inline506
    t437 = inline507
    var t438 string = t436 + t437
    var t439 string
    var inline482 float32 = 0.75
    var inline483 float64 = 4
    var inline486 float32 = 1
    var inline487 float64 = 5
    var inline488 bool = inline482 < inline486
    var inline489 bool = inline483 < inline487
    var inline490 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline488)
    var inline491 string = "left<1?=" + inline490
    var inline492 string = inline491 + ",right<5?="
    var inline493 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline489)
    var inline494 string = inline492 + inline493
    t439 = inline494
    var t440 string = t438 + t439
    var t441 string
    var inline468 float32 = 1.5
    var inline469 float64 = 7.25
    var inline472 float32 = 1
    var inline473 float64 = 5
    var inline474 bool = inline468 < inline472
    var inline475 bool = inline469 < inline473
    var inline476 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline474)
    var inline477 string = "left<1?=" + inline476
    var inline478 string = inline477 + ",right<5?="
    var inline479 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline475)
    var inline480 string = inline478 + inline479
    t441 = inline480
    var message__20 string = t440 + t441
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline465)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__161 float64) string {
    var t447 string = _goml_runtime_core_float64_to_string(self__161)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t450 string = _goml_runtime_core_bool_to_string(self__148)
    return t450
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
