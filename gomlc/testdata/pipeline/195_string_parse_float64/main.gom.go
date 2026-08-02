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
    var value__1 float64 = x157
    var valid__0 bool = x156
    var t170 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(valid__0)
    _goml_runtime_core_string_println(t170)
    var t171 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t171)
    var mtmp160 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x161 bool = mtmp160._0
    var x162 float64 = mtmp160._1
    var rounded__3 float64 = x162
    var rounded_valid__2 bool = x161
    var t172 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(rounded_valid__2)
    _goml_runtime_core_string_println(t172)
    var t173 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(rounded__3)
    _goml_runtime_core_string_println(t173)
    var mtmp165 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x166 bool = mtmp165._0
    var x167 float64 = mtmp165._1
    var fallback__5 float64 = x167
    var invalid__4 bool = x166
    var t174 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(invalid__4)
    _goml_runtime_core_string_println(t174)
    var t175 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(fallback__5)
    _goml_runtime_core_string_println(t175)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv178 string
    var t179 string = _goml_runtime_core_bool_to_string(self__37)
    retv178 = t179
    return retv178
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv181 string
    var t182 string = _goml_runtime_core_float64_to_string(self__50)
    retv181 = t182
    return retv181
}

func main() {
    main0()
}
