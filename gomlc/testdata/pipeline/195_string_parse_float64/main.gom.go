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
    var mtmp177 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x178 bool = mtmp177._0
    var x179 float64 = mtmp177._1
    var t192 string
    var inline239 string = _goml_runtime_core_bool_to_string(x178)
    t192 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline236)
    var t193 string
    var inline234 string = _goml_runtime_core_float64_to_string(x179)
    t193 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline231)
    var mtmp182 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x183 bool = mtmp182._0
    var x184 float64 = mtmp182._1
    var t194 string
    var inline229 string = _goml_runtime_core_bool_to_string(x183)
    t194 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline226)
    var t195 string
    var inline224 string = _goml_runtime_core_float64_to_string(x184)
    t195 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline221)
    var mtmp187 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x188 bool = mtmp187._0
    var x189 float64 = mtmp187._1
    var t196 string
    var inline219 string = _goml_runtime_core_bool_to_string(x188)
    t196 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline216)
    var t197 string
    var inline214 string = _goml_runtime_core_float64_to_string(x189)
    t197 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
