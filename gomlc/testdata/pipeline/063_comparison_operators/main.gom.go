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
    var t213 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t214 string = "10 < 20: " + t213
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline309)
    var greater__4 bool = b__1 > a__0
    var t215 string
    var inline307 string = _goml_runtime_core_bool_to_string(greater__4)
    t215 = inline307
    var t216 string = "20 > 10: " + t215
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline304)
    var less_eq1__5 bool = a__0 <= b__1
    var t217 string
    var inline302 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t217 = inline302
    var t218 string = "10 <= 20: " + t217
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline299)
    var less_eq2__6 bool = a__0 <= c__2
    var t219 string
    var inline297 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t219 = inline297
    var t220 string = "10 <= 10: " + t219
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline294)
    var greater_eq1__7 bool = b__1 >= a__0
    var t221 string
    var inline292 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t221 = inline292
    var t222 string = "20 >= 10: " + t221
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline289)
    var greater_eq2__8 bool = c__2 >= a__0
    var t223 string
    var inline287 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t223 = inline287
    var t224 string = "10 >= 10: " + t223
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline284)
    var eq1__9 bool = a__0 == c__2
    var t225 string
    var inline282 string = _goml_runtime_core_bool_to_string(eq1__9)
    t225 = inline282
    var t226 string = "10 == 10: " + t225
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline279)
    var eq2__10 bool = a__0 == b__1
    var t227 string
    var inline277 string = _goml_runtime_core_bool_to_string(eq2__10)
    t227 = inline277
    var t228 string = "10 == 20: " + t227
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline274)
    var neq1__11 bool = a__0 != b__1
    var t229 string
    var inline272 string = _goml_runtime_core_bool_to_string(neq1__11)
    t229 = inline272
    var t230 string = "10 != 20: " + t229
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline269)
    var neq2__12 bool = a__0 != c__2
    var t231 string
    var inline267 string = _goml_runtime_core_bool_to_string(neq2__12)
    t231 = inline267
    var t232 string = "10 != 10: " + t231
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t234 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t235 string = "2.71 < 3.14: " + t234
    var inline357 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline357)
    var greater__17 bool = x__13 > y__14
    var t236 string
    var inline355 string = _goml_runtime_core_bool_to_string(greater__17)
    t236 = inline355
    var t237 string = "3.14 > 2.71: " + t236
    var inline352 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline352)
    var less_eq1__18 bool = y__14 <= x__13
    var t238 string
    var inline350 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t238 = inline350
    var t239 string = "2.71 <= 3.14: " + t238
    var inline347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline347)
    var less_eq2__19 bool = x__13 <= z__15
    var t240 string
    var inline345 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t240 = inline345
    var t241 string = "3.14 <= 3.14: " + t240
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline342)
    var greater_eq1__20 bool = x__13 >= y__14
    var t242 string
    var inline340 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t242 = inline340
    var t243 string = "3.14 >= 2.71: " + t242
    var inline337 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
    _goml_runtime_core_string_println(inline337)
    var greater_eq2__21 bool = z__15 >= x__13
    var t244 string
    var inline335 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t244 = inline335
    var t245 string = "3.14 >= 3.14: " + t244
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
    _goml_runtime_core_string_println(inline332)
    var eq1__22 bool = x__13 == z__15
    var t246 string
    var inline330 string = _goml_runtime_core_bool_to_string(eq1__22)
    t246 = inline330
    var t247 string = "3.14 == 3.14: " + t246
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline327)
    var eq2__23 bool = x__13 == y__14
    var t248 string
    var inline325 string = _goml_runtime_core_bool_to_string(eq2__23)
    t248 = inline325
    var t249 string = "3.14 == 2.71: " + t248
    var inline322 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline322)
    var neq1__24 bool = x__13 != y__14
    var t250 string
    var inline320 string = _goml_runtime_core_bool_to_string(neq1__24)
    t250 = inline320
    var t251 string = "3.14 != 2.71: " + t250
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline317)
    var neq2__25 bool = x__13 != z__15
    var t252 string
    var inline315 string = _goml_runtime_core_bool_to_string(neq2__25)
    t252 = inline315
    var t253 string = "3.14 != 3.14: " + t252
    var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
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

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t260 string = _goml_runtime_core_bool_to_string(self__64)
    return t260
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
