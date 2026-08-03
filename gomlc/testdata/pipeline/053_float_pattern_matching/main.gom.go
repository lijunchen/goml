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
        var x177 string = event__0.(Sample32)._0
        var x178 float32 = event__0.(Sample32)._1
        var t188 string
        var inline223 string = _goml_runtime_core_float32_to_string(x178)
        t188 = inline223
        var t189 string = x177 + t188
        return t189
    case Sample64:
        var x179 string = event__0.(Sample64)._0
        var x180 float64 = event__0.(Sample64)._1
        var t190 string
        var inline225 string = _goml_runtime_core_float64_to_string(x180)
        t190 = inline225
        var t191 string = x179 + t190
        return t191
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
    var t200 string = summarize(first__15)
    var t201 string = summarize(second__16)
    var t202 string = t200 + t201
    var t203 string
    var inline268 string = "f64="
    var inline272 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline273 string = inline268 + inline272
    t203 = inline273
    var t204 string = t202 + t203
    var t205 string
    var inline248 float32 = 0.75
    var inline249 float64 = 4
    var inline252 float32 = 1
    var inline253 float64 = 5
    var inline254 bool = inline248 < inline252
    var inline255 bool = inline249 < inline253
    var inline256 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline254)
    var inline257 string = "left<1?=" + inline256
    var inline258 string = inline257 + ",right<5?="
    var inline259 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline255)
    var inline260 string = inline258 + inline259
    t205 = inline260
    var t206 string = t204 + t205
    var t207 string
    var inline234 float32 = 1.5
    var inline235 float64 = 7.25
    var inline238 float32 = 1
    var inline239 float64 = 5
    var inline240 bool = inline234 < inline238
    var inline241 bool = inline235 < inline239
    var inline242 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline240)
    var inline243 string = "left<1?=" + inline242
    var inline244 string = inline243 + ",right<5?="
    var inline245 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline241)
    var inline246 string = inline244 + inline245
    t207 = inline246
    var message__20 string = t206 + t207
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t213 string = _goml_runtime_core_float64_to_string(self__79)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t216 string = _goml_runtime_core_bool_to_string(self__66)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
