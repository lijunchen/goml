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
    var t203 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t204 string = "10 < 20: " + t203
    println__T_string(t204)
    var greater__4 bool = b__1 > a__0
    var t205 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t206 string = "20 > 10: " + t205
    println__T_string(t206)
    var less_eq1__5 bool = a__0 <= b__1
    var t207 string
    var inline310 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t207 = inline310
    var t208 string = "10 <= 20: " + t207
    var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline307)
    var less_eq2__6 bool = a__0 <= c__2
    var t209 string
    var inline305 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t209 = inline305
    var t210 string = "10 <= 10: " + t209
    var inline302 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline302)
    var greater_eq1__7 bool = b__1 >= a__0
    var t211 string
    var inline300 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t211 = inline300
    var t212 string = "20 >= 10: " + t211
    var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline297)
    var greater_eq2__8 bool = c__2 >= a__0
    var t213 string
    var inline295 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t213 = inline295
    var t214 string = "10 >= 10: " + t213
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline292)
    var eq1__9 bool
    var inline290 bool = a__0 == c__2
    eq1__9 = inline290
    var t215 string
    var inline288 string = _goml_runtime_core_bool_to_string(eq1__9)
    t215 = inline288
    var t216 string = "10 == 10: " + t215
    var inline285 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline285)
    var eq2__10 bool
    var inline283 bool = a__0 == b__1
    eq2__10 = inline283
    var t217 string
    var inline281 string = _goml_runtime_core_bool_to_string(eq2__10)
    t217 = inline281
    var t218 string = "10 == 20: " + t217
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline278)
    var t219 bool
    var inline276 bool = a__0 == b__1
    t219 = inline276
    var neq1__11 bool = !t219
    var t220 string
    var inline274 string = _goml_runtime_core_bool_to_string(neq1__11)
    t220 = inline274
    var t221 string = "10 != 20: " + t220
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline271)
    var t222 bool
    var inline269 bool = a__0 == c__2
    t222 = inline269
    var neq2__12 bool = !t222
    var t223 string
    var inline267 string = _goml_runtime_core_bool_to_string(neq2__12)
    t223 = inline267
    var t224 string = "10 != 10: " + t223
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t226 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t227 string = "2.71 < 3.14: " + t226
    println__T_string(t227)
    var greater__17 bool = x__13 > y__14
    var t228 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t229 string = "3.14 > 2.71: " + t228
    println__T_string(t229)
    var less_eq1__18 bool = y__14 <= x__13
    var t230 string
    var inline358 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t230 = inline358
    var t231 string = "2.71 <= 3.14: " + t230
    var inline355 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline355)
    var less_eq2__19 bool = x__13 <= z__15
    var t232 string
    var inline353 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t232 = inline353
    var t233 string = "3.14 <= 3.14: " + t232
    var inline350 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t233)
    _goml_runtime_core_string_println(inline350)
    var greater_eq1__20 bool = x__13 >= y__14
    var t234 string
    var inline348 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t234 = inline348
    var t235 string = "3.14 >= 2.71: " + t234
    var inline345 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline345)
    var greater_eq2__21 bool = z__15 >= x__13
    var t236 string
    var inline343 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t236 = inline343
    var t237 string = "3.14 >= 3.14: " + t236
    var inline340 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline340)
    var eq1__22 bool
    var inline338 bool = x__13 == z__15
    eq1__22 = inline338
    var t238 string
    var inline336 string = _goml_runtime_core_bool_to_string(eq1__22)
    t238 = inline336
    var t239 string = "3.14 == 3.14: " + t238
    var inline333 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline333)
    var eq2__23 bool
    var inline331 bool = x__13 == y__14
    eq2__23 = inline331
    var t240 string
    var inline329 string = _goml_runtime_core_bool_to_string(eq2__23)
    t240 = inline329
    var t241 string = "3.14 == 2.71: " + t240
    var inline326 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline326)
    var t242 bool
    var inline324 bool = x__13 == y__14
    t242 = inline324
    var neq1__24 bool = !t242
    var t243 string
    var inline322 string = _goml_runtime_core_bool_to_string(neq1__24)
    t243 = inline322
    var t244 string = "3.14 != 2.71: " + t243
    var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline319)
    var t245 bool
    var inline317 bool = x__13 == z__15
    t245 = inline317
    var neq2__25 bool = !t245
    var t246 string
    var inline315 string = _goml_runtime_core_bool_to_string(neq2__25)
    t246 = inline315
    var t247 string = "3.14 != 3.14: " + t246
    var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline312)
    return struct{}{}
}

func main0() struct{} {
    var inline368 string = "=== Integer Comparisons ==="
    var inline369 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline368)
    _goml_runtime_core_string_println(inline369)
    test_int_comparisons()
    var inline364 string = ""
    var inline365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline364)
    _goml_runtime_core_string_println(inline365)
    var inline360 string = "=== Float Comparisons ==="
    var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline360)
    _goml_runtime_core_string_println(inline361)
    test_float_comparisons()
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t250 string
    t250 = value__31
    _goml_runtime_core_string_println(t250)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t254 string = _goml_runtime_core_bool_to_string(self__66)
    return t254
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
