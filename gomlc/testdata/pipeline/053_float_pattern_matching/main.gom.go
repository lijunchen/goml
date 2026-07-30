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
    var retv116 string
    var jp118 string
    switch event__0.(type) {
    case Sample32:
        var x108 string = event__0.(Sample32)._0
        var x109 float32 = event__0.(Sample32)._1
        var value__2 float32 = x109
        var label__1 string = x108
        var t119 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t120 string = label__1 + t119
        jp118 = t120
    case Sample64:
        var x110 string = event__0.(Sample64)._0
        var x111 float64 = event__0.(Sample64)._1
        var value__4 float64 = x111
        var label__3 string = x110
        var t121 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t122 string = label__3 + t121
        jp118 = t122
    default:
        panic("non-exhaustive match")
    }
    retv116 = jp118
    return retv116
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv124 string
    var x112 float32 = values__5._0
    var x113 float64 = values__5._1
    var right__7 float64 = x113
    var left__6 float32 = x112
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t125 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t126 string = "left<1?=" + t125
    var t127 string = t126 + ",right<5?="
    var t128 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t129 string = t127 + t128
    retv124 = t129
    return retv124
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
    var t131 string = summarize(first__15)
    var t132 string = summarize(second__16)
    var t133 string = t131 + t132
    var t134 string = summarize(third__17)
    var t135 string = t133 + t134
    var t136 string = compare(tuple__18)
    var t137 string = t135 + t136
    var t138 string = compare(tuple_other__19)
    var message__20 string = t137 + t138
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv140 string
    var t141 string = _goml_runtime_core_float32_to_string(self__49)
    retv140 = t141
    return retv140
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv143 string
    var t144 string = _goml_runtime_core_float64_to_string(self__50)
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv146 string
    var t147 string = _goml_runtime_core_bool_to_string(self__37)
    retv146 = t147
    return retv146
}

func println__T_string(value__1 string) struct{} {
    var t149 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv152 string
    retv152 = self__38
    return retv152
}

func main() {
    main0()
}
