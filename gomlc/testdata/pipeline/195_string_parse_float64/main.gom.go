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

type Ordering int32

func main0() struct{} {
    var mtmp411 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x412 bool = mtmp411._0
    var x413 float64 = mtmp411._1
    var t426 string
    var inline473 string = _goml_runtime_core_bool_to_string(x412)
    t426 = inline473
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline470)
    var t427 string
    var inline468 string = _goml_runtime_core_float64_to_string(x413)
    t427 = inline468
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline465)
    var mtmp416 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x417 bool = mtmp416._0
    var x418 float64 = mtmp416._1
    var t428 string
    var inline463 string = _goml_runtime_core_bool_to_string(x417)
    t428 = inline463
    var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline460)
    var t429 string
    var inline458 string = _goml_runtime_core_float64_to_string(x418)
    t429 = inline458
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline455)
    var mtmp421 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x422 bool = mtmp421._0
    var x423 float64 = mtmp421._1
    var t430 string
    var inline453 string = _goml_runtime_core_bool_to_string(x422)
    t430 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline450)
    var t431 string
    var inline448 string = _goml_runtime_core_float64_to_string(x423)
    t431 = inline448
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
