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
    var mtmp182 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x183 bool = mtmp182._0
    var x184 float64 = mtmp182._1
    var t197 string
    var inline244 string = _goml_runtime_core_bool_to_string(x183)
    t197 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline241)
    var t198 string
    var inline239 string = _goml_runtime_core_float64_to_string(x184)
    t198 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline236)
    var mtmp187 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x188 bool = mtmp187._0
    var x189 float64 = mtmp187._1
    var t199 string
    var inline234 string = _goml_runtime_core_bool_to_string(x188)
    t199 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline231)
    var t200 string
    var inline229 string = _goml_runtime_core_float64_to_string(x189)
    t200 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline226)
    var mtmp192 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x193 bool = mtmp192._0
    var x194 float64 = mtmp192._1
    var t201 string
    var inline224 string = _goml_runtime_core_bool_to_string(x193)
    t201 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline221)
    var t202 string
    var inline219 string = _goml_runtime_core_float64_to_string(x194)
    t202 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
