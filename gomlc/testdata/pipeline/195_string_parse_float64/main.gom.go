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
    var mtmp172 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x173 bool = mtmp172._0
    var x174 float64 = mtmp172._1
    var t187 string
    var inline234 string = _goml_runtime_core_bool_to_string(x173)
    t187 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline231)
    var t188 string
    var inline229 string = _goml_runtime_core_float64_to_string(x174)
    t188 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline226)
    var mtmp177 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x178 bool = mtmp177._0
    var x179 float64 = mtmp177._1
    var t189 string
    var inline224 string = _goml_runtime_core_bool_to_string(x178)
    t189 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline221)
    var t190 string
    var inline219 string = _goml_runtime_core_float64_to_string(x179)
    t190 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline216)
    var mtmp182 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x183 bool = mtmp182._0
    var x184 float64 = mtmp182._1
    var t191 string
    var inline214 string = _goml_runtime_core_bool_to_string(x183)
    t191 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline211)
    var t192 string
    var inline209 string = _goml_runtime_core_float64_to_string(x184)
    t192 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
