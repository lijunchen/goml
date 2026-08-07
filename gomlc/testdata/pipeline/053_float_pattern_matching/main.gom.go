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
        var x172 string = event__0.(Sample32)._0
        var x173 float32 = event__0.(Sample32)._1
        var t183 string
        var inline218 string = _goml_runtime_core_float32_to_string(x173)
        t183 = inline218
        var t184 string = x172 + t183
        return t184
    case Sample64:
        var x174 string = event__0.(Sample64)._0
        var x175 float64 = event__0.(Sample64)._1
        var t185 string
        var inline220 string = _goml_runtime_core_float64_to_string(x175)
        t185 = inline220
        var t186 string = x174 + t185
        return t186
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
    var t195 string = summarize(first__15)
    var t196 string = summarize(second__16)
    var t197 string = t195 + t196
    var t198 string
    var inline263 string = "f64="
    var inline267 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline268 string = inline263 + inline267
    t198 = inline268
    var t199 string = t197 + t198
    var t200 string
    var inline243 float32 = 0.75
    var inline244 float64 = 4
    var inline247 float32 = 1
    var inline248 float64 = 5
    var inline249 bool = inline243 < inline247
    var inline250 bool = inline244 < inline248
    var inline251 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline249)
    var inline252 string = "left<1?=" + inline251
    var inline253 string = inline252 + ",right<5?="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline250)
    var inline255 string = inline253 + inline254
    t200 = inline255
    var t201 string = t199 + t200
    var t202 string
    var inline229 float32 = 1.5
    var inline230 float64 = 7.25
    var inline233 float32 = 1
    var inline234 float64 = 5
    var inline235 bool = inline229 < inline233
    var inline236 bool = inline230 < inline234
    var inline237 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline235)
    var inline238 string = "left<1?=" + inline237
    var inline239 string = inline238 + ",right<5?="
    var inline240 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline236)
    var inline241 string = inline239 + inline240
    t202 = inline241
    var message__20 string = t201 + t202
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t208 string = _goml_runtime_core_float64_to_string(self__79)
    return t208
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t211 string = _goml_runtime_core_bool_to_string(self__66)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
