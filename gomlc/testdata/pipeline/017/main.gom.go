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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

func _goml_m_trait__impl_i_ToString_i__o_int_c_int_q__i_to__string(self__0 Tuple2_3int_3int) string {
    var retv112 string
    retv112 = "(?, ?)"
    return retv112
}

func main0() struct{} {
    var x__1 int = 123
    var t114 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__1)
    println__T_string(t114)
    var x__2 bool = true
    var t115 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x__2)
    println__T_string(t115)
    var x__3 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 3,
        _1: 4,
    }
    var t116 string = _goml_m_trait__impl_i_ToString_i__o_int_c_int_q__i_to__string(x__3)
    println__T_string(t116)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int_to_string(self__40)
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv124 string
    var t125 string = _goml_runtime_core_bool_to_string(self__37)
    retv124 = t125
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv127 string
    retv127 = self__38
    return retv127
}

func main() {
    main0()
}
