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
    var mtmp152 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x153 bool = mtmp152._0
    var x154 float64 = mtmp152._1
    var value__1 float64 = x154
    var valid__0 bool = x153
    var t167 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(valid__0)
    _goml_runtime_core_string_println(t167)
    var t168 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    var mtmp157 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x158 bool = mtmp157._0
    var x159 float64 = mtmp157._1
    var rounded__3 float64 = x159
    var rounded_valid__2 bool = x158
    var t169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(rounded_valid__2)
    _goml_runtime_core_string_println(t169)
    var t170 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(rounded__3)
    _goml_runtime_core_string_println(t170)
    var mtmp162 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x163 bool = mtmp162._0
    var x164 float64 = mtmp162._1
    var fallback__5 float64 = x164
    var invalid__4 bool = x163
    var t171 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(invalid__4)
    _goml_runtime_core_string_println(t171)
    var t172 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(fallback__5)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv175 string
    var t176 string = _goml_runtime_core_bool_to_string(self__37)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv178 string
    var t179 string = _goml_runtime_core_float64_to_string(self__50)
    retv178 = t179
    return retv178
}

func main() {
    main0()
}
