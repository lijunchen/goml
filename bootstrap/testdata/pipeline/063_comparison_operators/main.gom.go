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
    var t90 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t91 string = "10 < 20: " + t90
    println__T_string(t91)
    var greater__4 bool = b__1 > a__0
    var t92 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t93 string = "20 > 10: " + t92
    println__T_string(t93)
    var less_eq1__5 bool = a__0 <= b__1
    var t94 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t95 string = "10 <= 20: " + t94
    println__T_string(t95)
    var less_eq2__6 bool = a__0 <= c__2
    var t96 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t97 string = "10 <= 10: " + t96
    println__T_string(t97)
    var greater_eq1__7 bool = b__1 >= a__0
    var t98 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t99 string = "20 >= 10: " + t98
    println__T_string(t99)
    var greater_eq2__8 bool = c__2 >= a__0
    var t100 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t101 string = "10 >= 10: " + t100
    println__T_string(t101)
    var eq1__9 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var t102 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t103 string = "10 == 10: " + t102
    println__T_string(t103)
    var eq2__10 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var t104 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t105 string = "10 == 20: " + t104
    println__T_string(t105)
    var t106 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var neq1__11 bool = !t106
    var t107 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t108 string = "10 != 20: " + t107
    println__T_string(t108)
    var t109 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var neq2__12 bool = !t109
    var t110 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t111 string = "10 != 10: " + t110
    println__T_string(t111)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t113 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t114 string = "2.71 < 3.14: " + t113
    println__T_string(t114)
    var greater__17 bool = x__13 > y__14
    var t115 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t116 string = "3.14 > 2.71: " + t115
    println__T_string(t116)
    var less_eq1__18 bool = y__14 <= x__13
    var t117 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t118 string = "2.71 <= 3.14: " + t117
    println__T_string(t118)
    var less_eq2__19 bool = x__13 <= z__15
    var t119 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t120 string = "3.14 <= 3.14: " + t119
    println__T_string(t120)
    var greater_eq1__20 bool = x__13 >= y__14
    var t121 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t122 string = "3.14 >= 2.71: " + t121
    println__T_string(t122)
    var greater_eq2__21 bool = z__15 >= x__13
    var t123 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t124 string = "3.14 >= 3.14: " + t123
    println__T_string(t124)
    var eq1__22 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var t125 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t126 string = "3.14 == 3.14: " + t125
    println__T_string(t126)
    var eq2__23 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var t127 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t128 string = "3.14 == 2.71: " + t127
    println__T_string(t128)
    var t129 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var neq1__24 bool = !t129
    var t130 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t131 string = "3.14 != 2.71: " + t130
    println__T_string(t131)
    var t132 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var neq2__25 bool = !t132
    var t133 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t134 string = "3.14 != 3.14: " + t133
    println__T_string(t134)
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
    var t137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t137)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv140 string
    var t141 string = _goml_runtime_core_bool_to_string(self__37)
    retv140 = t141
    return retv140
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv143 bool
    var t144 bool = self__65 == other__66
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var retv146 bool
    var t147 bool = self__79 == other__80
    retv146 = t147
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv149 string
    retv149 = self__38
    return retv149
}

func main() {
    main0()
}
