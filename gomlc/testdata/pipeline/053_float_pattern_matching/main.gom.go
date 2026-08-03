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
        var x136 string = event__0.(Sample32)._0
        var x137 float32 = event__0.(Sample32)._1
        var t147 string
        var inline182 string = _goml_runtime_core_float32_to_string(x137)
        t147 = inline182
        var t148 string = x136 + t147
        return t148
    case Sample64:
        var x138 string = event__0.(Sample64)._0
        var x139 float64 = event__0.(Sample64)._1
        var t149 string
        var inline184 string = _goml_runtime_core_float64_to_string(x139)
        t149 = inline184
        var t150 string = x138 + t149
        return t150
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
    var t159 string = summarize(first__15)
    var t160 string = summarize(second__16)
    var t161 string = t159 + t160
    var t162 string
    var inline227 string = "f64="
    var inline231 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(third_value__14)
    var inline232 string = inline227 + inline231
    t162 = inline232
    var t163 string = t161 + t162
    var t164 string
    var inline207 float32 = 0.75
    var inline208 float64 = 4
    var inline211 float32 = 1
    var inline212 float64 = 5
    var inline213 bool = inline207 < inline211
    var inline214 bool = inline208 < inline212
    var inline215 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline213)
    var inline216 string = "left<1?=" + inline215
    var inline217 string = inline216 + ",right<5?="
    var inline218 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline214)
    var inline219 string = inline217 + inline218
    t164 = inline219
    var t165 string = t163 + t164
    var t166 string
    var inline193 float32 = 1.5
    var inline194 float64 = 7.25
    var inline197 float32 = 1
    var inline198 float64 = 5
    var inline199 bool = inline193 < inline197
    var inline200 bool = inline194 < inline198
    var inline201 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline199)
    var inline202 string = "left<1?=" + inline201
    var inline203 string = inline202 + ",right<5?="
    var inline204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline200)
    var inline205 string = inline203 + inline204
    t166 = inline205
    var message__20 string = t165 + t166
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t172 string = _goml_runtime_core_float64_to_string(self__79)
    return t172
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t175 string = _goml_runtime_core_bool_to_string(self__66)
    return t175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
