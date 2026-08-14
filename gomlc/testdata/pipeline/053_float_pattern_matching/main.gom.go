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
        var x187 string = event__0.(Sample32)._0
        var x188 float32 = event__0.(Sample32)._1
        var t198 string
        var inline233 string = _goml_runtime_core_float32_to_string(x188)
        t198 = inline233
        var t199 string = x187 + t198
        return t199
    case Sample64:
        var x189 string = event__0.(Sample64)._0
        var x190 float64 = event__0.(Sample64)._1
        var t200 string
        var inline235 string = _goml_runtime_core_float64_to_string(x190)
        t200 = inline235
        var t201 string = x189 + t200
        return t201
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
    var t210 string = summarize(first__15)
    var t211 string = summarize(second__16)
    var t212 string = t210 + t211
    var t213 string
    var inline278 string = "f64="
    var inline282 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline283 string = inline278 + inline282
    t213 = inline283
    var t214 string = t212 + t213
    var t215 string
    var inline258 float32 = 0.75
    var inline259 float64 = 4
    var inline262 float32 = 1
    var inline263 float64 = 5
    var inline264 bool = inline258 < inline262
    var inline265 bool = inline259 < inline263
    var inline266 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline264)
    var inline267 string = "left<1?=" + inline266
    var inline268 string = inline267 + ",right<5?="
    var inline269 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline265)
    var inline270 string = inline268 + inline269
    t215 = inline270
    var t216 string = t214 + t215
    var t217 string
    var inline244 float32 = 1.5
    var inline245 float64 = 7.25
    var inline248 float32 = 1
    var inline249 float64 = 5
    var inline250 bool = inline244 < inline248
    var inline251 bool = inline245 < inline249
    var inline252 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline250)
    var inline253 string = "left<1?=" + inline252
    var inline254 string = inline253 + ",right<5?="
    var inline255 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline251)
    var inline256 string = inline254 + inline255
    t217 = inline256
    var message__20 string = t216 + t217
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t223 string = _goml_runtime_core_float64_to_string(self__77)
    return t223
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t226 string = _goml_runtime_core_bool_to_string(self__64)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
