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
    var t134 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t135 string = "10 < 20: " + t134
    println__T_string(t135)
    var greater__4 bool = b__1 > a__0
    var t136 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t137 string = "20 > 10: " + t136
    println__T_string(t137)
    var less_eq1__5 bool = a__0 <= b__1
    var t138 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t139 string = "10 <= 20: " + t138
    println__T_string(t139)
    var less_eq2__6 bool = a__0 <= c__2
    var t140 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t141 string = "10 <= 10: " + t140
    println__T_string(t141)
    var greater_eq1__7 bool = b__1 >= a__0
    var t142 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t143 string = "20 >= 10: " + t142
    println__T_string(t143)
    var greater_eq2__8 bool = c__2 >= a__0
    var t144 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t145 string = "10 >= 10: " + t144
    println__T_string(t145)
    var eq1__9 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var t146 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t147 string = "10 == 10: " + t146
    println__T_string(t147)
    var eq2__10 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var t148 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t149 string = "10 == 20: " + t148
    println__T_string(t149)
    var t150 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var neq1__11 bool = !t150
    var t151 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t152 string = "10 != 20: " + t151
    println__T_string(t152)
    var t153 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var neq2__12 bool = !t153
    var t154 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t155 string = "10 != 10: " + t154
    println__T_string(t155)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t157 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t158 string = "2.71 < 3.14: " + t157
    println__T_string(t158)
    var greater__17 bool = x__13 > y__14
    var t159 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t160 string = "3.14 > 2.71: " + t159
    println__T_string(t160)
    var less_eq1__18 bool = y__14 <= x__13
    var t161 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t162 string = "2.71 <= 3.14: " + t161
    println__T_string(t162)
    var less_eq2__19 bool = x__13 <= z__15
    var t163 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t164 string = "3.14 <= 3.14: " + t163
    println__T_string(t164)
    var greater_eq1__20 bool = x__13 >= y__14
    var t165 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t166 string = "3.14 >= 2.71: " + t165
    println__T_string(t166)
    var greater_eq2__21 bool = z__15 >= x__13
    var t167 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t168 string = "3.14 >= 3.14: " + t167
    println__T_string(t168)
    var eq1__22 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var t169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t170 string = "3.14 == 3.14: " + t169
    println__T_string(t170)
    var eq2__23 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var t171 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t172 string = "3.14 == 2.71: " + t171
    println__T_string(t172)
    var t173 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var neq1__24 bool = !t173
    var t174 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t175 string = "3.14 != 2.71: " + t174
    println__T_string(t175)
    var t176 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var neq2__25 bool = !t176
    var t177 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t178 string = "3.14 != 3.14: " + t177
    println__T_string(t178)
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
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv184 string
    var t185 string = _goml_runtime_core_bool_to_string(self__37)
    retv184 = t185
    return retv184
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv187 bool
    var t188 bool = self__65 == other__66
    retv187 = t188
    return retv187
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var retv190 bool
    var t191 bool = self__79 == other__80
    retv190 = t191
    return retv190
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv193 string
    retv193 = self__38
    return retv193
}

func main() {
    main0()
}
