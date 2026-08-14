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
    var mtmp408 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("3.125")
    var x409 bool = mtmp408._0
    var x410 float64 = mtmp408._1
    var t423 string
    var inline470 string = _goml_runtime_core_bool_to_string(x409)
    t423 = inline470
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline467)
    var t424 string
    var inline465 string = _goml_runtime_core_float64_to_string(x410)
    t424 = inline465
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline462)
    var mtmp413 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float32("3.14")
    var x414 bool = mtmp413._0
    var x415 float64 = mtmp413._1
    var t425 string
    var inline460 string = _goml_runtime_core_bool_to_string(x414)
    t425 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline457)
    var t426 string
    var inline455 string = _goml_runtime_core_float64_to_string(x415)
    t426 = inline455
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline452)
    var mtmp418 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x419 bool = mtmp418._0
    var x420 float64 = mtmp418._1
    var t427 string
    var inline450 string = _goml_runtime_core_bool_to_string(x419)
    t427 = inline450
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline447)
    var t428 string
    var inline445 string = _goml_runtime_core_float64_to_string(x420)
    t428 = inline445
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline442)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
