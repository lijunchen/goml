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
    var t181 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t182 string = "10 < 20: " + t181
    println__T_string(t182)
    var greater__4 bool = b__1 > a__0
    var t183 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t184 string = "20 > 10: " + t183
    println__T_string(t184)
    var less_eq1__5 bool = a__0 <= b__1
    var t185 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t186 string = "10 <= 20: " + t185
    println__T_string(t186)
    var less_eq2__6 bool = a__0 <= c__2
    var t187 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t188 string = "10 <= 10: " + t187
    println__T_string(t188)
    var greater_eq1__7 bool = b__1 >= a__0
    var t189 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t190 string = "20 >= 10: " + t189
    println__T_string(t190)
    var greater_eq2__8 bool = c__2 >= a__0
    var t191 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t192 string = "10 >= 10: " + t191
    println__T_string(t192)
    var eq1__9 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var t193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t194 string = "10 == 10: " + t193
    println__T_string(t194)
    var eq2__10 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var t195 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t196 string = "10 == 20: " + t195
    println__T_string(t196)
    var t197 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var neq1__11 bool = !t197
    var t198 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t199 string = "10 != 20: " + t198
    println__T_string(t199)
    var t200 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var neq2__12 bool = !t200
    var t201 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t202 string = "10 != 10: " + t201
    println__T_string(t202)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t205 string = "2.71 < 3.14: " + t204
    println__T_string(t205)
    var greater__17 bool = x__13 > y__14
    var t206 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t207 string = "3.14 > 2.71: " + t206
    println__T_string(t207)
    var less_eq1__18 bool = y__14 <= x__13
    var t208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t209 string = "2.71 <= 3.14: " + t208
    println__T_string(t209)
    var less_eq2__19 bool = x__13 <= z__15
    var t210 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t211 string = "3.14 <= 3.14: " + t210
    println__T_string(t211)
    var greater_eq1__20 bool = x__13 >= y__14
    var t212 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t213 string = "3.14 >= 2.71: " + t212
    println__T_string(t213)
    var greater_eq2__21 bool = z__15 >= x__13
    var t214 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t215 string = "3.14 >= 3.14: " + t214
    println__T_string(t215)
    var eq1__22 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var t216 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t217 string = "3.14 == 3.14: " + t216
    println__T_string(t217)
    var eq2__23 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var t218 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t219 string = "3.14 == 2.71: " + t218
    println__T_string(t219)
    var t220 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var neq1__24 bool = !t220
    var t221 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t222 string = "3.14 != 2.71: " + t221
    println__T_string(t222)
    var t223 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var neq2__25 bool = !t223
    var t224 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t225 string = "3.14 != 3.14: " + t224
    println__T_string(t225)
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
    var t228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t232 string = _goml_runtime_core_bool_to_string(self__37)
    return t232
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t235 bool = self__65 == other__66
    return t235
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var t238 bool = self__79 == other__80
    return t238
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
