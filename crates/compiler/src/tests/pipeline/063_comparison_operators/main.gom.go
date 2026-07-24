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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func test_int_comparisons() struct{} {
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t87 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t88 string = "10 < 20: " + t87
    println__T_string(t88)
    var greater__4 bool = b__1 > a__0
    var t89 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t90 string = "20 > 10: " + t89
    println__T_string(t90)
    var less_eq1__5 bool = a__0 <= b__1
    var t91 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t92 string = "10 <= 20: " + t91
    println__T_string(t92)
    var less_eq2__6 bool = a__0 <= c__2
    var t93 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t94 string = "10 <= 10: " + t93
    println__T_string(t94)
    var greater_eq1__7 bool = b__1 >= a__0
    var t95 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t96 string = "20 >= 10: " + t95
    println__T_string(t96)
    var greater_eq2__8 bool = c__2 >= a__0
    var t97 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t98 string = "10 >= 10: " + t97
    println__T_string(t98)
    var eq1__9 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var t99 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t100 string = "10 == 10: " + t99
    println__T_string(t100)
    var eq2__10 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var t101 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t102 string = "10 == 20: " + t101
    println__T_string(t102)
    var t103 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var neq1__11 bool = !t103
    var t104 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t105 string = "10 != 20: " + t104
    println__T_string(t105)
    var t106 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var neq2__12 bool = !t106
    var t107 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t108 string = "10 != 10: " + t107
    println__T_string(t108)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t110 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t111 string = "2.71 < 3.14: " + t110
    println__T_string(t111)
    var greater__17 bool = x__13 > y__14
    var t112 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t113 string = "3.14 > 2.71: " + t112
    println__T_string(t113)
    var less_eq1__18 bool = y__14 <= x__13
    var t114 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t115 string = "2.71 <= 3.14: " + t114
    println__T_string(t115)
    var less_eq2__19 bool = x__13 <= z__15
    var t116 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t117 string = "3.14 <= 3.14: " + t116
    println__T_string(t117)
    var greater_eq1__20 bool = x__13 >= y__14
    var t118 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t119 string = "3.14 >= 2.71: " + t118
    println__T_string(t119)
    var greater_eq2__21 bool = z__15 >= x__13
    var t120 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t121 string = "3.14 >= 3.14: " + t120
    println__T_string(t121)
    var eq1__22 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var t122 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t123 string = "3.14 == 3.14: " + t122
    println__T_string(t123)
    var eq2__23 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var t124 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t125 string = "3.14 == 2.71: " + t124
    println__T_string(t125)
    var t126 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var neq1__24 bool = !t126
    var t127 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t128 string = "3.14 != 2.71: " + t127
    println__T_string(t128)
    var t129 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var neq2__25 bool = !t129
    var t130 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t131 string = "3.14 != 3.14: " + t130
    println__T_string(t131)
    return struct{}{}
}

func main0() struct{} {
    println__T_string("=== Integer Comparisons ===")
    test_int_comparisons()
    println__T_string("")
    println__T_string("=== Float Comparisons ===")
    test_float_comparisons()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv137 string
    var t138 string = _goml_runtime_core_bool_to_string(self__36)
    retv137 = t138
    return retv137
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv140 bool
    var t141 bool = self__61 == other__62
    retv140 = t141
    return retv140
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__75 float64, other__76 float64) bool {
    var retv143 bool
    var t144 bool = self__75 == other__76
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv146 string
    retv146 = self__37
    return retv146
}

func main() {
    main0()
}
