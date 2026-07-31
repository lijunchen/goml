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
    var t178 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t179 string = "10 < 20: " + t178
    println__T_string(t179)
    var greater__4 bool = b__1 > a__0
    var t180 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t181 string = "20 > 10: " + t180
    println__T_string(t181)
    var less_eq1__5 bool = a__0 <= b__1
    var t182 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t183 string = "10 <= 20: " + t182
    println__T_string(t183)
    var less_eq2__6 bool = a__0 <= c__2
    var t184 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t185 string = "10 <= 10: " + t184
    println__T_string(t185)
    var greater_eq1__7 bool = b__1 >= a__0
    var t186 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t187 string = "20 >= 10: " + t186
    println__T_string(t187)
    var greater_eq2__8 bool = c__2 >= a__0
    var t188 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t189 string = "10 >= 10: " + t188
    println__T_string(t189)
    var eq1__9 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var t190 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t191 string = "10 == 10: " + t190
    println__T_string(t191)
    var eq2__10 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var t192 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t193 string = "10 == 20: " + t192
    println__T_string(t193)
    var t194 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, b__1)
    var neq1__11 bool = !t194
    var t195 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t196 string = "10 != 20: " + t195
    println__T_string(t196)
    var t197 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__0, c__2)
    var neq2__12 bool = !t197
    var t198 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t199 string = "10 != 10: " + t198
    println__T_string(t199)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t201 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t202 string = "2.71 < 3.14: " + t201
    println__T_string(t202)
    var greater__17 bool = x__13 > y__14
    var t203 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t204 string = "3.14 > 2.71: " + t203
    println__T_string(t204)
    var less_eq1__18 bool = y__14 <= x__13
    var t205 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t206 string = "2.71 <= 3.14: " + t205
    println__T_string(t206)
    var less_eq2__19 bool = x__13 <= z__15
    var t207 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t208 string = "3.14 <= 3.14: " + t207
    println__T_string(t208)
    var greater_eq1__20 bool = x__13 >= y__14
    var t209 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t210 string = "3.14 >= 2.71: " + t209
    println__T_string(t210)
    var greater_eq2__21 bool = z__15 >= x__13
    var t211 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t212 string = "3.14 >= 3.14: " + t211
    println__T_string(t212)
    var eq1__22 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var t213 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t214 string = "3.14 == 3.14: " + t213
    println__T_string(t214)
    var eq2__23 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var t215 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t216 string = "3.14 == 2.71: " + t215
    println__T_string(t216)
    var t217 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, y__14)
    var neq1__24 bool = !t217
    var t218 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t219 string = "3.14 != 2.71: " + t218
    println__T_string(t219)
    var t220 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(x__13, z__15)
    var neq2__25 bool = !t220
    var t221 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t222 string = "3.14 != 3.14: " + t221
    println__T_string(t222)
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
    var t225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t225)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv228 string
    var t229 string = _goml_runtime_core_bool_to_string(self__37)
    retv228 = t229
    return retv228
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv231 bool
    var t232 bool = self__65 == other__66
    retv231 = t232
    return retv231
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var retv234 bool
    var t235 bool = self__79 == other__80
    retv234 = t235
    return retv234
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv237 string
    retv237 = self__38
    return retv237
}

func main() {
    main0()
}
