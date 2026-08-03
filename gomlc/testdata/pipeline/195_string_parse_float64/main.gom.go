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
    var mtmp136 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x137 bool = mtmp136._0
    var x138 float64 = mtmp136._1
    var t151 string
    var inline198 string = _goml_runtime_core_bool_to_string(x137)
    t151 = inline198
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t151)
    _goml_runtime_core_string_println(inline195)
    var t152 string
    var inline193 string = _goml_runtime_core_float64_to_string(x138)
    t152 = inline193
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
    _goml_runtime_core_string_println(inline190)
    var mtmp141 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x142 bool = mtmp141._0
    var x143 float64 = mtmp141._1
    var t153 string
    var inline188 string = _goml_runtime_core_bool_to_string(x142)
    t153 = inline188
    var inline185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline185)
    var t154 string
    var inline183 string = _goml_runtime_core_float64_to_string(x143)
    t154 = inline183
    var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline180)
    var mtmp146 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x147 bool = mtmp146._0
    var x148 float64 = mtmp146._1
    var t155 string
    var inline178 string = _goml_runtime_core_bool_to_string(x147)
    t155 = inline178
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline175)
    var t156 string
    var inline173 string = _goml_runtime_core_float64_to_string(x148)
    t156 = inline173
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t156)
    _goml_runtime_core_string_println(inline170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
