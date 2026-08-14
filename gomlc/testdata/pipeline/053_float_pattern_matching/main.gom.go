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
        var x182 string = event__0.(Sample32)._0
        var x183 float32 = event__0.(Sample32)._1
        var t193 string
        var inline228 string = _goml_runtime_core_float32_to_string(x183)
        t193 = inline228
        var t194 string = x182 + t193
        return t194
    case Sample64:
        var x184 string = event__0.(Sample64)._0
        var x185 float64 = event__0.(Sample64)._1
        var t195 string
        var inline230 string = _goml_runtime_core_float64_to_string(x185)
        t195 = inline230
        var t196 string = x184 + t195
        return t196
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
    var t205 string = summarize(first__15)
    var t206 string = summarize(second__16)
    var t207 string = t205 + t206
    var t208 string
    var inline273 string = "f64="
    var inline277 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline278 string = inline273 + inline277
    t208 = inline278
    var t209 string = t207 + t208
    var t210 string
    var inline253 float32 = 0.75
    var inline254 float64 = 4
    var inline257 float32 = 1
    var inline258 float64 = 5
    var inline259 bool = inline253 < inline257
    var inline260 bool = inline254 < inline258
    var inline261 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline259)
    var inline262 string = "left<1?=" + inline261
    var inline263 string = inline262 + ",right<5?="
    var inline264 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline260)
    var inline265 string = inline263 + inline264
    t210 = inline265
    var t211 string = t209 + t210
    var t212 string
    var inline239 float32 = 1.5
    var inline240 float64 = 7.25
    var inline243 float32 = 1
    var inline244 float64 = 5
    var inline245 bool = inline239 < inline243
    var inline246 bool = inline240 < inline244
    var inline247 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline245)
    var inline248 string = "left<1?=" + inline247
    var inline249 string = inline248 + ",right<5?="
    var inline250 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline246)
    var inline251 string = inline249 + inline250
    t212 = inline251
    var message__20 string = t211 + t212
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t218 string = _goml_runtime_core_float64_to_string(self__77)
    return t218
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t221 string = _goml_runtime_core_bool_to_string(self__64)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
