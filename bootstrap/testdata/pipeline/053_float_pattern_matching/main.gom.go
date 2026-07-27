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
    var retv72 string
    var jp74 string
    switch event__0.(type) {
    case Sample32:
        var x64 string = event__0.(Sample32)._0
        var x65 float32 = event__0.(Sample32)._1
        var value__2 float32 = x65
        var label__1 string = x64
        var t75 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t76 string = label__1 + t75
        jp74 = t76
    case Sample64:
        var x66 string = event__0.(Sample64)._0
        var x67 float64 = event__0.(Sample64)._1
        var value__4 float64 = x67
        var label__3 string = x66
        var t77 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t78 string = label__3 + t77
        jp74 = t78
    default:
        panic("non-exhaustive match")
    }
    retv72 = jp74
    return retv72
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv80 string
    var x68 float32 = values__5._0
    var x69 float64 = values__5._1
    var right__7 float64 = x69
    var left__6 float32 = x68
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t81 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t82 string = "left<1?=" + t81
    var t83 string = t82 + ",right<5?="
    var t84 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t85 string = t83 + t84
    retv80 = t85
    return retv80
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
    var t87 string = summarize(first__15)
    var t88 string = summarize(second__16)
    var t89 string = t87 + t88
    var t90 string = summarize(third__17)
    var t91 string = t89 + t90
    var t92 string = compare(tuple__18)
    var t93 string = t91 + t92
    var t94 string = compare(tuple_other__19)
    var message__20 string = t93 + t94
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_float32_to_string(self__49)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv99 string
    var t100 string = _goml_runtime_core_float64_to_string(self__50)
    retv99 = t100
    return retv99
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv102 string
    var t103 string = _goml_runtime_core_bool_to_string(self__37)
    retv102 = t103
    return retv102
}

func println__T_string(value__1 string) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv108 string
    retv108 = self__38
    return retv108
}

func main() {
    main0()
}
