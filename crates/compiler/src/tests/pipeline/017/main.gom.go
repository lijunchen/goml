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
    var retv11 string
    retv11 = "(?, ?)"
    return retv11
}

func main0() struct{} {
    var x__1 int32 = 123
    var t13 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__1)
    println__T_string(t13)
    var x__2 bool = true
    var t14 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x__2)
    println__T_string(t14)
    var x__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 3,
        _1: 4,
    }
    var t15 string = _goml_m_trait__impl_i_ToString_i__o_int32_c_int32_q__i_to__string(x__3)
    println__T_string(t15)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t17 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t17)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv20 string
    var t21 string = _goml_runtime_core_int32_to_string(self__13)
    retv20 = t21
    return retv20
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv23 string
    var t24 string = _goml_runtime_core_bool_to_string(self__8)
    retv23 = t24
    return retv23
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv26 string
    retv26 = self__9
    return retv26
}

func main() {
    main0()
}
