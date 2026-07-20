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
    var mtmp61 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x62 bool = mtmp61._0
    var x63 float64 = mtmp61._1
    var value__1 float64 = x63
    var valid__0 bool = x62
    var t76 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(valid__0)
    _goml_runtime_core_string_println(t76)
    var t77 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    var mtmp66 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x67 bool = mtmp66._0
    var x68 float64 = mtmp66._1
    var rounded__3 float64 = x68
    var rounded_valid__2 bool = x67
    var t78 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(rounded_valid__2)
    _goml_runtime_core_string_println(t78)
    var t79 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(rounded__3)
    _goml_runtime_core_string_println(t79)
    var mtmp71 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x72 bool = mtmp71._0
    var x73 float64 = mtmp71._1
    var fallback__5 float64 = x73
    var invalid__4 bool = x72
    var t80 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(invalid__4)
    _goml_runtime_core_string_println(t80)
    var t81 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(fallback__5)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv84 string
    var t85 string = _goml_runtime_core_bool_to_string(self__36)
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__48 float64) string {
    var retv87 string
    var t88 string = _goml_runtime_core_float64_to_string(self__48)
    retv87 = t88
    return retv87
}

func main() {
    main0()
}
