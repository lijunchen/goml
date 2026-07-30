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
    var t94 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t95 string = "10 < 20: " + t94
    println__T_string(t95)
    var greater__4 bool = b__1 > a__0
    var t96 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t97 string = "20 > 10: " + t96
    println__T_string(t97)
    var less_eq1__5 bool = a__0 <= b__1
    var t98 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t99 string = "10 <= 20: " + t98
    println__T_string(t99)
    var less_eq2__6 bool = a__0 <= c__2
    var t100 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t101 string = "10 <= 10: " + t100
    println__T_string(t101)
    var greater_eq1__7 bool = b__1 >= a__0
    var t102 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t103 string = "20 >= 10: " + t102
    println__T_string(t103)
    var greater_eq2__8 bool = c__2 >= a__0
    var t104 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t105 string = "10 >= 10: " + t104
    println__T_string(t105)
    var eq1__9 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var t106 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t107 string = "10 == 10: " + t106
    println__T_string(t107)
    var eq2__10 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var t108 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t109 string = "10 == 20: " + t108
    println__T_string(t109)
    var t110 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var neq1__11 bool = !t110
    var t111 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t112 string = "10 != 20: " + t111
    println__T_string(t112)
    var t113 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var neq2__12 bool = !t113
    var t114 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t115 string = "10 != 10: " + t114
    println__T_string(t115)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t117 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t118 string = "2.71 < 3.14: " + t117
    println__T_string(t118)
    var greater__17 bool = x__13 > y__14
    var t119 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t120 string = "3.14 > 2.71: " + t119
    println__T_string(t120)
    var less_eq1__18 bool = y__14 <= x__13
    var t121 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t122 string = "2.71 <= 3.14: " + t121
    println__T_string(t122)
    var less_eq2__19 bool = x__13 <= z__15
    var t123 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t124 string = "3.14 <= 3.14: " + t123
    println__T_string(t124)
    var greater_eq1__20 bool = x__13 >= y__14
    var t125 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t126 string = "3.14 >= 2.71: " + t125
    println__T_string(t126)
    var greater_eq2__21 bool = z__15 >= x__13
    var t127 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t128 string = "3.14 >= 3.14: " + t127
    println__T_string(t128)
    var eq1__22 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var t129 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t130 string = "3.14 == 3.14: " + t129
    println__T_string(t130)
    var eq2__23 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var t131 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t132 string = "3.14 == 2.71: " + t131
    println__T_string(t132)
    var t133 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var neq1__24 bool = !t133
    var t134 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t135 string = "3.14 != 2.71: " + t134
    println__T_string(t135)
    var t136 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var neq2__25 bool = !t136
    var t137 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t138 string = "3.14 != 3.14: " + t137
    println__T_string(t138)
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
    var t141 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv144 string
    var t145 string = _goml_runtime_core_bool_to_string(self__37)
    retv144 = t145
    return retv144
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv147 bool
    var t148 bool = self__65 == other__66
    retv147 = t148
    return retv147
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var retv150 bool
    var t151 bool = self__79 == other__80
    retv150 = t151
    return retv150
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv153 string
    retv153 = self__38
    return retv153
}

func main() {
    main0()
}
