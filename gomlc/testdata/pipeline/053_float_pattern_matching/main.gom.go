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
    var retv160 string
    var jp162 string
    switch event__0.(type) {
    case Sample32:
        var x152 string = event__0.(Sample32)._0
        var x153 float32 = event__0.(Sample32)._1
        var value__2 float32 = x153
        var label__1 string = x152
        var t163 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t164 string = label__1 + t163
        jp162 = t164
    case Sample64:
        var x154 string = event__0.(Sample64)._0
        var x155 float64 = event__0.(Sample64)._1
        var value__4 float64 = x155
        var label__3 string = x154
        var t165 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t166 string = label__3 + t165
        jp162 = t166
    default:
        panic("non-exhaustive match")
    }
    retv160 = jp162
    return retv160
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv168 string
    var x156 float32 = values__5._0
    var x157 float64 = values__5._1
    var right__7 float64 = x157
    var left__6 float32 = x156
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t170 string = "left<1?=" + t169
    var t171 string = t170 + ",right<5?="
    var t172 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t173 string = t171 + t172
    retv168 = t173
    return retv168
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
    var t175 string = summarize(first__15)
    var t176 string = summarize(second__16)
    var t177 string = t175 + t176
    var t178 string = summarize(third__17)
    var t179 string = t177 + t178
    var t180 string = compare(tuple__18)
    var t181 string = t179 + t180
    var t182 string = compare(tuple_other__19)
    var message__20 string = t181 + t182
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv184 string
    var t185 string = _goml_runtime_core_float32_to_string(self__49)
    retv184 = t185
    return retv184
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv187 string
    var t188 string = _goml_runtime_core_float64_to_string(self__50)
    retv187 = t188
    return retv187
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv190 string
    var t191 string = _goml_runtime_core_bool_to_string(self__37)
    retv190 = t191
    return retv190
}

func println__T_string(value__1 string) struct{} {
    var t193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv196 string
    retv196 = self__38
    return retv196
}

func main() {
    main0()
}
