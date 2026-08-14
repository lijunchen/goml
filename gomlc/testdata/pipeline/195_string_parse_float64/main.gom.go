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

func _goml_runtime_core_string_parse_float32(value string) Tuple2_4bool_7float64 {
    var parsed float64
    var err error
    parsed, err = _goml_strconv.ParseFloat(value, 32)
    return Tuple2_4bool_7float64{
        _0: err == nil,
        _1: parsed,
    }
}

func _goml_runtime_core_string_parse_float64(value string) Tuple2_4bool_7float64 {
    var parsed float64
    var err error
    parsed, err = _goml_strconv.ParseFloat(value, 64)
    return Tuple2_4bool_7float64{
        _0: err == nil,
        _1: parsed,
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_7float64 struct {
    _0 bool
    _1 float64
}

func main0() struct{} {
    var mtmp187 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x188 bool = mtmp187._0
    var x189 float64 = mtmp187._1
    var t202 string
    var inline249 string = _goml_runtime_core_bool_to_string(x188)
    t202 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline246)
    var t203 string
    var inline244 string = _goml_runtime_core_float64_to_string(x189)
    t203 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline241)
    var mtmp192 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x193 bool = mtmp192._0
    var x194 float64 = mtmp192._1
    var t204 string
    var inline239 string = _goml_runtime_core_bool_to_string(x193)
    t204 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline236)
    var t205 string
    var inline234 string = _goml_runtime_core_float64_to_string(x194)
    t205 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline231)
    var mtmp197 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x198 bool = mtmp197._0
    var x199 float64 = mtmp197._1
    var t206 string
    var inline229 string = _goml_runtime_core_bool_to_string(x198)
    t206 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline226)
    var t207 string
    var inline224 string = _goml_runtime_core_float64_to_string(x199)
    t207 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
