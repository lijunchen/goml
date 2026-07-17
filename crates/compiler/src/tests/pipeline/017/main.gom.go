package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_5int32_5int32 struct {
    _0 int32
    _1 int32
}

func _goml_m_trait__impl_i_ToString_i__o_int32_c_int32_q__i_to__string(self__0 Tuple2_5int32_5int32) string {
    var retv65 string
    retv65 = "(?, ?)"
    return retv65
}

func main0() struct{} {
    var x__1 int32 = 123
    var t67 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__1)
    println__T_string(t67)
    var x__2 bool = true
    var t68 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x__2)
    println__T_string(t68)
    var x__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 3,
        _1: 4,
    }
    var t69 string = _goml_m_trait__impl_i_ToString_i__o_int32_c_int32_q__i_to__string(x__3)
    println__T_string(t69)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__41)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv77 string
    var t78 string = _goml_runtime_core_bool_to_string(self__36)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv80 string
    retv80 = self__37
    return retv80
}

func main() {
    main0()
}
