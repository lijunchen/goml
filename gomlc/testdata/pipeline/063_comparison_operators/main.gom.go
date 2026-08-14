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
    var t208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t209 string = "10 < 20: " + t208
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline304)
    var greater__4 bool = b__1 > a__0
    var t210 string
    var inline302 string = _goml_runtime_core_bool_to_string(greater__4)
    t210 = inline302
    var t211 string = "20 > 10: " + t210
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline299)
    var less_eq1__5 bool = a__0 <= b__1
    var t212 string
    var inline297 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t212 = inline297
    var t213 string = "10 <= 20: " + t212
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline294)
    var less_eq2__6 bool = a__0 <= c__2
    var t214 string
    var inline292 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t214 = inline292
    var t215 string = "10 <= 10: " + t214
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline289)
    var greater_eq1__7 bool = b__1 >= a__0
    var t216 string
    var inline287 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t216 = inline287
    var t217 string = "20 >= 10: " + t216
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline284)
    var greater_eq2__8 bool = c__2 >= a__0
    var t218 string
    var inline282 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t218 = inline282
    var t219 string = "10 >= 10: " + t218
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline279)
    var eq1__9 bool = a__0 == c__2
    var t220 string
    var inline277 string = _goml_runtime_core_bool_to_string(eq1__9)
    t220 = inline277
    var t221 string = "10 == 10: " + t220
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline274)
    var eq2__10 bool = a__0 == b__1
    var t222 string
    var inline272 string = _goml_runtime_core_bool_to_string(eq2__10)
    t222 = inline272
    var t223 string = "10 == 20: " + t222
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline269)
    var neq1__11 bool = a__0 != b__1
    var t224 string
    var inline267 string = _goml_runtime_core_bool_to_string(neq1__11)
    t224 = inline267
    var t225 string = "10 != 20: " + t224
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline264)
    var neq2__12 bool = a__0 != c__2
    var t226 string
    var inline262 string = _goml_runtime_core_bool_to_string(neq2__12)
    t226 = inline262
    var t227 string = "10 != 10: " + t226
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline259)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t229 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t230 string = "2.71 < 3.14: " + t229
    var inline352 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline352)
    var greater__17 bool = x__13 > y__14
    var t231 string
    var inline350 string = _goml_runtime_core_bool_to_string(greater__17)
    t231 = inline350
    var t232 string = "3.14 > 2.71: " + t231
    var inline347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline347)
    var less_eq1__18 bool = y__14 <= x__13
    var t233 string
    var inline345 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t233 = inline345
    var t234 string = "2.71 <= 3.14: " + t233
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline342)
    var less_eq2__19 bool = x__13 <= z__15
    var t235 string
    var inline340 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t235 = inline340
    var t236 string = "3.14 <= 3.14: " + t235
    var inline337 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline337)
    var greater_eq1__20 bool = x__13 >= y__14
    var t237 string
    var inline335 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t237 = inline335
    var t238 string = "3.14 >= 2.71: " + t237
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline332)
    var greater_eq2__21 bool = z__15 >= x__13
    var t239 string
    var inline330 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t239 = inline330
    var t240 string = "3.14 >= 3.14: " + t239
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline327)
    var eq1__22 bool = x__13 == z__15
    var t241 string
    var inline325 string = _goml_runtime_core_bool_to_string(eq1__22)
    t241 = inline325
    var t242 string = "3.14 == 3.14: " + t241
    var inline322 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline322)
    var eq2__23 bool = x__13 == y__14
    var t243 string
    var inline320 string = _goml_runtime_core_bool_to_string(eq2__23)
    t243 = inline320
    var t244 string = "3.14 == 2.71: " + t243
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline317)
    var neq1__24 bool = x__13 != y__14
    var t245 string
    var inline315 string = _goml_runtime_core_bool_to_string(neq1__24)
    t245 = inline315
    var t246 string = "3.14 != 2.71: " + t245
    var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
    _goml_runtime_core_string_println(inline312)
    var neq2__25 bool = x__13 != z__15
    var t247 string
    var inline310 string = _goml_runtime_core_bool_to_string(neq2__25)
    t247 = inline310
    var t248 string = "3.14 != 3.14: " + t247
    var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t248)
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

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t255 string = _goml_runtime_core_bool_to_string(self__64)
    return t255
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
