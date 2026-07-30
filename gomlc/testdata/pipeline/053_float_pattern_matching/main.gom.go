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
    var retv76 string
    var jp78 string
    switch event__0.(type) {
    case Sample32:
        var x68 string = event__0.(Sample32)._0
        var x69 float32 = event__0.(Sample32)._1
        var value__2 float32 = x69
        var label__1 string = x68
        var t79 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t80 string = label__1 + t79
        jp78 = t80
    case Sample64:
        var x70 string = event__0.(Sample64)._0
        var x71 float64 = event__0.(Sample64)._1
        var value__4 float64 = x71
        var label__3 string = x70
        var t81 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t82 string = label__3 + t81
        jp78 = t82
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv84 string
    var x72 float32 = values__5._0
    var x73 float64 = values__5._1
    var right__7 float64 = x73
    var left__6 float32 = x72
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t85 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t86 string = "left<1?=" + t85
    var t87 string = t86 + ",right<5?="
    var t88 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t89 string = t87 + t88
    retv84 = t89
    return retv84
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
    var third__17 FloatEvent = Sample64{
        _0: "f64=",
        _1: third_value__14,
    }
    var tuple__18 Tuple2_7float32_7float64 = Tuple2_7float32_7float64{
        _0: 0.75,
        _1: 4,
    }
    var tuple_other__19 Tuple2_7float32_7float64 = Tuple2_7float32_7float64{
        _0: 1.5,
        _1: 7.25,
    }
    var t91 string = summarize(first__15)
    var t92 string = summarize(second__16)
    var t93 string = t91 + t92
    var t94 string = summarize(third__17)
    var t95 string = t93 + t94
    var t96 string = compare(tuple__18)
    var t97 string = t95 + t96
    var t98 string = compare(tuple_other__19)
    var message__20 string = t97 + t98
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv100 string
    var t101 string = _goml_runtime_core_float32_to_string(self__49)
    retv100 = t101
    return retv100
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv103 string
    var t104 string = _goml_runtime_core_float64_to_string(self__50)
    retv103 = t104
    return retv103
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv106 string
    var t107 string = _goml_runtime_core_bool_to_string(self__37)
    retv106 = t107
    return retv106
}

func println__T_string(value__1 string) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv112 string
    retv112 = self__38
    return retv112
}

func main() {
    main0()
}
