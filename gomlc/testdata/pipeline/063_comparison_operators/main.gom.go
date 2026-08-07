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
    var t198 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t199 string = "10 < 20: " + t198
    println__T_string(t199)
    var greater__4 bool = b__1 > a__0
    var t200 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t201 string = "20 > 10: " + t200
    println__T_string(t201)
    var less_eq1__5 bool = a__0 <= b__1
    var t202 string
    var inline305 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t202 = inline305
    var t203 string = "10 <= 20: " + t202
    var inline302 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline302)
    var less_eq2__6 bool = a__0 <= c__2
    var t204 string
    var inline300 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t204 = inline300
    var t205 string = "10 <= 10: " + t204
    var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline297)
    var greater_eq1__7 bool = b__1 >= a__0
    var t206 string
    var inline295 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t206 = inline295
    var t207 string = "20 >= 10: " + t206
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline292)
    var greater_eq2__8 bool = c__2 >= a__0
    var t208 string
    var inline290 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t208 = inline290
    var t209 string = "10 >= 10: " + t208
    var inline287 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline287)
    var eq1__9 bool
    var inline285 bool = a__0 == c__2
    eq1__9 = inline285
    var t210 string
    var inline283 string = _goml_runtime_core_bool_to_string(eq1__9)
    t210 = inline283
    var t211 string = "10 == 10: " + t210
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline280)
    var eq2__10 bool
    var inline278 bool = a__0 == b__1
    eq2__10 = inline278
    var t212 string
    var inline276 string = _goml_runtime_core_bool_to_string(eq2__10)
    t212 = inline276
    var t213 string = "10 == 20: " + t212
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline273)
    var t214 bool
    var inline271 bool = a__0 == b__1
    t214 = inline271
    var neq1__11 bool = !t214
    var t215 string
    var inline269 string = _goml_runtime_core_bool_to_string(neq1__11)
    t215 = inline269
    var t216 string = "10 != 20: " + t215
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline266)
    var t217 bool
    var inline264 bool = a__0 == c__2
    t217 = inline264
    var neq2__12 bool = !t217
    var t218 string
    var inline262 string = _goml_runtime_core_bool_to_string(neq2__12)
    t218 = inline262
    var t219 string = "10 != 10: " + t218
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline259)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t221 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t222 string = "2.71 < 3.14: " + t221
    println__T_string(t222)
    var greater__17 bool = x__13 > y__14
    var t223 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t224 string = "3.14 > 2.71: " + t223
    println__T_string(t224)
    var less_eq1__18 bool = y__14 <= x__13
    var t225 string
    var inline353 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t225 = inline353
    var t226 string = "2.71 <= 3.14: " + t225
    var inline350 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline350)
    var less_eq2__19 bool = x__13 <= z__15
    var t227 string
    var inline348 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t227 = inline348
    var t228 string = "3.14 <= 3.14: " + t227
    var inline345 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline345)
    var greater_eq1__20 bool = x__13 >= y__14
    var t229 string
    var inline343 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t229 = inline343
    var t230 string = "3.14 >= 2.71: " + t229
    var inline340 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline340)
    var greater_eq2__21 bool = z__15 >= x__13
    var t231 string
    var inline338 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t231 = inline338
    var t232 string = "3.14 >= 3.14: " + t231
    var inline335 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline335)
    var eq1__22 bool
    var inline333 bool = x__13 == z__15
    eq1__22 = inline333
    var t233 string
    var inline331 string = _goml_runtime_core_bool_to_string(eq1__22)
    t233 = inline331
    var t234 string = "3.14 == 3.14: " + t233
    var inline328 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline328)
    var eq2__23 bool
    var inline326 bool = x__13 == y__14
    eq2__23 = inline326
    var t235 string
    var inline324 string = _goml_runtime_core_bool_to_string(eq2__23)
    t235 = inline324
    var t236 string = "3.14 == 2.71: " + t235
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline321)
    var t237 bool
    var inline319 bool = x__13 == y__14
    t237 = inline319
    var neq1__24 bool = !t237
    var t238 string
    var inline317 string = _goml_runtime_core_bool_to_string(neq1__24)
    t238 = inline317
    var t239 string = "3.14 != 2.71: " + t238
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline314)
    var t240 bool
    var inline312 bool = x__13 == z__15
    t240 = inline312
    var neq2__25 bool = !t240
    var t241 string
    var inline310 string = _goml_runtime_core_bool_to_string(neq2__25)
    t241 = inline310
    var t242 string = "3.14 != 3.14: " + t241
    var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline307)
    return struct{}{}
}

func main0() struct{} {
    var inline363 string = "=== Integer Comparisons ==="
    var inline364 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline363)
    _goml_runtime_core_string_println(inline364)
    test_int_comparisons()
    var inline359 string = ""
    var inline360 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline359)
    _goml_runtime_core_string_println(inline360)
    var inline355 string = "=== Float Comparisons ==="
    var inline356 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline355)
    _goml_runtime_core_string_println(inline356)
    test_float_comparisons()
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t245 string
    t245 = value__31
    _goml_runtime_core_string_println(t245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t249 string = _goml_runtime_core_bool_to_string(self__66)
    return t249
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
