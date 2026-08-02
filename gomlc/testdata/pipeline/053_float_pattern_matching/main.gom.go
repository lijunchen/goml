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
    switch event__0.(type) {
    case Sample32:
        var x155 string = event__0.(Sample32)._0
        var x156 float32 = event__0.(Sample32)._1
        var t166 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(x156)
        var t167 string = x155 + t166
        return t167
    case Sample64:
        var x157 string = event__0.(Sample64)._0
        var x158 float64 = event__0.(Sample64)._1
        var t168 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(x158)
        var t169 string = x157 + t168
        return t169
    default:
        panic("non-exhaustive match")
    }
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var x159 float32 = values__5._0
    var x160 float64 = values__5._1
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = x159 < limit32__8
    var less_right__11 bool = x160 < limit64__9
    var t172 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t173 string = "left<1?=" + t172
    var t174 string = t173 + ",right<5?="
    var t175 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t176 string = t174 + t175
    return t176
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
    var t178 string = summarize(first__15)
    var t179 string = summarize(second__16)
    var t180 string = t178 + t179
    var t181 string = summarize(third__17)
    var t182 string = t180 + t181
    var t183 string = compare(tuple__18)
    var t184 string = t182 + t183
    var t185 string = compare(tuple_other__19)
    var message__20 string = t184 + t185
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var t188 string = _goml_runtime_core_float32_to_string(self__49)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var t191 string = _goml_runtime_core_float64_to_string(self__50)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t194 string = _goml_runtime_core_bool_to_string(self__37)
    return t194
}

func println__T_string(value__1 string) struct{} {
    var t196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
