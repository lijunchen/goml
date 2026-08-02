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
        var t166 string
        var inline201 string = _goml_runtime_core_float32_to_string(x156)
        t166 = inline201
        var t167 string = x155 + t166
        return t167
    case Sample64:
        var x157 string = event__0.(Sample64)._0
        var x158 float64 = event__0.(Sample64)._1
        var t168 string
        var inline203 string = _goml_runtime_core_float64_to_string(x158)
        t168 = inline203
        var t169 string = x157 + t168
        return t169
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
    var t178 string = summarize(first__15)
    var t179 string = summarize(second__16)
    var t180 string = t178 + t179
    var t181 string
    var inline246 string = "f64="
    var inline250 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline251 string = inline246 + inline250
    t181 = inline251
    var t182 string = t180 + t181
    var t183 string
    var inline226 float32 = 0.75
    var inline227 float64 = 4
    var inline230 float32 = 1
    var inline231 float64 = 5
    var inline232 bool = inline226 < inline230
    var inline233 bool = inline227 < inline231
    var inline234 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline232)
    var inline235 string = "left<1?=" + inline234
    var inline236 string = inline235 + ",right<5?="
    var inline237 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline233)
    var inline238 string = inline236 + inline237
    t183 = inline238
    var t184 string = t182 + t183
    var t185 string
    var inline212 float32 = 1.5
    var inline213 float64 = 7.25
    var inline216 float32 = 1
    var inline217 float64 = 5
    var inline218 bool = inline212 < inline216
    var inline219 bool = inline213 < inline217
    var inline220 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline218)
    var inline221 string = "left<1?=" + inline220
    var inline222 string = inline221 + ",right<5?="
    var inline223 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline219)
    var inline224 string = inline222 + inline223
    t185 = inline224
    var message__20 string = t184 + t185
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var t191 string = _goml_runtime_core_float64_to_string(self__50)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t194 string = _goml_runtime_core_bool_to_string(self__37)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
