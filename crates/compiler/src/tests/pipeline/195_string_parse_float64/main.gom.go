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
    return _goml_fmt.Sprintf("%g", x)
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
    var t71 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(valid__0)
    _goml_runtime_core_string_println(t71)
    var t72 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    var mtmp66 Tuple2_4bool_7float64 = _goml_runtime_core_string_parse_float64("not-a-number")
    var x67 bool = mtmp66._0
    var x68 float64 = mtmp66._1
    var fallback__3 float64 = x68
    var invalid__2 bool = x67
    var t73 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(invalid__2)
    _goml_runtime_core_string_println(t73)
    var t74 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(fallback__3)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv77 string
    var t78 string = _goml_runtime_core_bool_to_string(self__36)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__48 float64) string {
    var retv80 string
    var t81 string = _goml_runtime_core_float64_to_string(self__48)
    retv80 = t81
    return retv80
}

func main() {
    main0()
}
