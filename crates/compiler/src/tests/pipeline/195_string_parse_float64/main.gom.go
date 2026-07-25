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
    var mtmp64 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x65 bool = mtmp64._0
    var x66 float64 = mtmp64._1
    var value__1 float64 = x66
    var valid__0 bool = x65
    var t79 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(valid__0)
    _goml_runtime_core_string_println(t79)
    var t80 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    var mtmp69 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x70 bool = mtmp69._0
    var x71 float64 = mtmp69._1
    var rounded__3 float64 = x71
    var rounded_valid__2 bool = x70
    var t81 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(rounded_valid__2)
    _goml_runtime_core_string_println(t81)
    var t82 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(rounded__3)
    _goml_runtime_core_string_println(t82)
    var mtmp74 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x75 bool = mtmp74._0
    var x76 float64 = mtmp74._1
    var fallback__5 float64 = x76
    var invalid__4 bool = x75
    var t83 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(invalid__4)
    _goml_runtime_core_string_println(t83)
    var t84 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(fallback__5)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv87 string
    var t88 string = _goml_runtime_core_bool_to_string(self__37)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv90 string
    var t91 string = _goml_runtime_core_float64_to_string(self__50)
    retv90 = t91
    return retv90
}

func main() {
    main0()
}
