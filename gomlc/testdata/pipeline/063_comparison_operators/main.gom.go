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
    var t185 string
    var inline288 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t185 = inline288
    var t186 string = "10 <= 20: " + t185
    var inline285 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline285)
    var less_eq2__6 bool = a__0 <= c__2
    var t187 string
    var inline283 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t187 = inline283
    var t188 string = "10 <= 10: " + t187
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline280)
    var greater_eq1__7 bool = b__1 >= a__0
    var t189 string
    var inline278 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t189 = inline278
    var t190 string = "20 >= 10: " + t189
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline275)
    var greater_eq2__8 bool = c__2 >= a__0
    var t191 string
    var inline273 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t191 = inline273
    var t192 string = "10 >= 10: " + t191
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline270)
    var eq1__9 bool
    var inline268 bool = a__0 == c__2
    eq1__9 = inline268
    var t193 string
    var inline266 string = _goml_runtime_core_bool_to_string(eq1__9)
    t193 = inline266
    var t194 string = "10 == 10: " + t193
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline263)
    var eq2__10 bool
    var inline261 bool = a__0 == b__1
    eq2__10 = inline261
    var t195 string
    var inline259 string = _goml_runtime_core_bool_to_string(eq2__10)
    t195 = inline259
    var t196 string = "10 == 20: " + t195
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline256)
    var t197 bool
    var inline254 bool = a__0 == b__1
    t197 = inline254
    var neq1__11 bool = !t197
    var t198 string
    var inline252 string = _goml_runtime_core_bool_to_string(neq1__11)
    t198 = inline252
    var t199 string = "10 != 20: " + t198
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline249)
    var t200 bool
    var inline247 bool = a__0 == c__2
    t200 = inline247
    var neq2__12 bool = !t200
    var t201 string
    var inline245 string = _goml_runtime_core_bool_to_string(neq2__12)
    t201 = inline245
    var t202 string = "10 != 10: " + t201
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline242)
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
    var t208 string
    var inline336 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t208 = inline336
    var t209 string = "2.71 <= 3.14: " + t208
    var inline333 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline333)
    var less_eq2__19 bool = x__13 <= z__15
    var t210 string
    var inline331 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t210 = inline331
    var t211 string = "3.14 <= 3.14: " + t210
    var inline328 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline328)
    var greater_eq1__20 bool = x__13 >= y__14
    var t212 string
    var inline326 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t212 = inline326
    var t213 string = "3.14 >= 2.71: " + t212
    var inline323 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline323)
    var greater_eq2__21 bool = z__15 >= x__13
    var t214 string
    var inline321 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t214 = inline321
    var t215 string = "3.14 >= 3.14: " + t214
    var inline318 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline318)
    var eq1__22 bool
    var inline316 bool = x__13 == z__15
    eq1__22 = inline316
    var t216 string
    var inline314 string = _goml_runtime_core_bool_to_string(eq1__22)
    t216 = inline314
    var t217 string = "3.14 == 3.14: " + t216
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline311)
    var eq2__23 bool
    var inline309 bool = x__13 == y__14
    eq2__23 = inline309
    var t218 string
    var inline307 string = _goml_runtime_core_bool_to_string(eq2__23)
    t218 = inline307
    var t219 string = "3.14 == 2.71: " + t218
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline304)
    var t220 bool
    var inline302 bool = x__13 == y__14
    t220 = inline302
    var neq1__24 bool = !t220
    var t221 string
    var inline300 string = _goml_runtime_core_bool_to_string(neq1__24)
    t221 = inline300
    var t222 string = "3.14 != 2.71: " + t221
    var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline297)
    var t223 bool
    var inline295 bool = x__13 == z__15
    t223 = inline295
    var neq2__25 bool = !t223
    var t224 string
    var inline293 string = _goml_runtime_core_bool_to_string(neq2__25)
    t224 = inline293
    var t225 string = "3.14 != 3.14: " + t224
    var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline290)
    return struct{}{}
}

func main0() struct{} {
    var inline346 string = "=== Integer Comparisons ==="
    var inline347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline346)
    _goml_runtime_core_string_println(inline347)
    test_int_comparisons()
    var inline342 string = ""
    var inline343 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline342)
    _goml_runtime_core_string_println(inline343)
    var inline338 string = "=== Float Comparisons ==="
    var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline338)
    _goml_runtime_core_string_println(inline339)
    test_float_comparisons()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t228 string
    t228 = value__1
    _goml_runtime_core_string_println(t228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t232 string = _goml_runtime_core_bool_to_string(self__37)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
