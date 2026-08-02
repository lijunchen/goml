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
    var mtmp155 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x156 bool = mtmp155._0
    var x157 float64 = mtmp155._1
    var t170 string
    var inline217 string = _goml_runtime_core_bool_to_string(x156)
    t170 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline214)
    var t171 string
    var inline212 string = _goml_runtime_core_float64_to_string(x157)
    t171 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline209)
    var mtmp160 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x161 bool = mtmp160._0
    var x162 float64 = mtmp160._1
    var t172 string
    var inline207 string = _goml_runtime_core_bool_to_string(x161)
    t172 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline204)
    var t173 string
    var inline202 string = _goml_runtime_core_float64_to_string(x162)
    t173 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline199)
    var mtmp165 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x166 bool = mtmp165._0
    var x167 float64 = mtmp165._1
    var t174 string
    var inline197 string = _goml_runtime_core_bool_to_string(x166)
    t174 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline194)
    var t175 string
    var inline192 string = _goml_runtime_core_float64_to_string(x167)
    t175 = inline192
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
