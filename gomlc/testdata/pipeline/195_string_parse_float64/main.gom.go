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
    var mtmp108 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x109 bool = mtmp108._0
    var x110 float64 = mtmp108._1
    var value__1 float64 = x110
    var valid__0 bool = x109
    var t123 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(valid__0)
    _goml_runtime_core_string_println(t123)
    var t124 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t124)
    var mtmp113 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x114 bool = mtmp113._0
    var x115 float64 = mtmp113._1
    var rounded__3 float64 = x115
    var rounded_valid__2 bool = x114
    var t125 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(rounded_valid__2)
    _goml_runtime_core_string_println(t125)
    var t126 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(rounded__3)
    _goml_runtime_core_string_println(t126)
    var mtmp118 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x119 bool = mtmp118._0
    var x120 float64 = mtmp118._1
    var fallback__5 float64 = x120
    var invalid__4 bool = x119
    var t127 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(invalid__4)
    _goml_runtime_core_string_println(t127)
    var t128 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(fallback__5)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv131 string
    var t132 string = _goml_runtime_core_bool_to_string(self__37)
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv134 string
    var t135 string = _goml_runtime_core_float64_to_string(self__50)
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
