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
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline294)
    var greater__4 bool = b__1 > a__0
    var t200 string
    var inline292 string = _goml_runtime_core_bool_to_string(greater__4)
    t200 = inline292
    var t201 string = "20 > 10: " + t200
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline289)
    var less_eq1__5 bool = a__0 <= b__1
    var t202 string
    var inline287 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t202 = inline287
    var t203 string = "10 <= 20: " + t202
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline284)
    var less_eq2__6 bool = a__0 <= c__2
    var t204 string
    var inline282 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t204 = inline282
    var t205 string = "10 <= 10: " + t204
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline279)
    var greater_eq1__7 bool = b__1 >= a__0
    var t206 string
    var inline277 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t206 = inline277
    var t207 string = "20 >= 10: " + t206
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline274)
    var greater_eq2__8 bool = c__2 >= a__0
    var t208 string
    var inline272 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t208 = inline272
    var t209 string = "10 >= 10: " + t208
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline269)
    var eq1__9 bool = a__0 == c__2
    var t210 string
    var inline267 string = _goml_runtime_core_bool_to_string(eq1__9)
    t210 = inline267
    var t211 string = "10 == 10: " + t210
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline264)
    var eq2__10 bool = a__0 == b__1
    var t212 string
    var inline262 string = _goml_runtime_core_bool_to_string(eq2__10)
    t212 = inline262
    var t213 string = "10 == 20: " + t212
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline259)
    var neq1__11 bool = a__0 != b__1
    var t214 string
    var inline257 string = _goml_runtime_core_bool_to_string(neq1__11)
    t214 = inline257
    var t215 string = "10 != 20: " + t214
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline254)
    var neq2__12 bool = a__0 != c__2
    var t216 string
    var inline252 string = _goml_runtime_core_bool_to_string(neq2__12)
    t216 = inline252
    var t217 string = "10 != 10: " + t216
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline249)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t219 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t220 string = "2.71 < 3.14: " + t219
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline342)
    var greater__17 bool = x__13 > y__14
    var t221 string
    var inline340 string = _goml_runtime_core_bool_to_string(greater__17)
    t221 = inline340
    var t222 string = "3.14 > 2.71: " + t221
    var inline337 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline337)
    var less_eq1__18 bool = y__14 <= x__13
    var t223 string
    var inline335 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t223 = inline335
    var t224 string = "2.71 <= 3.14: " + t223
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline332)
    var less_eq2__19 bool = x__13 <= z__15
    var t225 string
    var inline330 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t225 = inline330
    var t226 string = "3.14 <= 3.14: " + t225
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline327)
    var greater_eq1__20 bool = x__13 >= y__14
    var t227 string
    var inline325 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t227 = inline325
    var t228 string = "3.14 >= 2.71: " + t227
    var inline322 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline322)
    var greater_eq2__21 bool = z__15 >= x__13
    var t229 string
    var inline320 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t229 = inline320
    var t230 string = "3.14 >= 3.14: " + t229
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline317)
    var eq1__22 bool = x__13 == z__15
    var t231 string
    var inline315 string = _goml_runtime_core_bool_to_string(eq1__22)
    t231 = inline315
    var t232 string = "3.14 == 3.14: " + t231
    var inline312 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline312)
    var eq2__23 bool = x__13 == y__14
    var t233 string
    var inline310 string = _goml_runtime_core_bool_to_string(eq2__23)
    t233 = inline310
    var t234 string = "3.14 == 2.71: " + t233
    var inline307 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline307)
    var neq1__24 bool = x__13 != y__14
    var t235 string
    var inline305 string = _goml_runtime_core_bool_to_string(neq1__24)
    t235 = inline305
    var t236 string = "3.14 != 2.71: " + t235
    var inline302 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline302)
    var neq2__25 bool = x__13 != z__15
    var t237 string
    var inline300 string = _goml_runtime_core_bool_to_string(neq2__25)
    t237 = inline300
    var t238 string = "3.14 != 3.14: " + t237
    var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline297)
    return struct{}{}
}

func main0() struct{} {
    var inline353 string = "=== Integer Comparisons ==="
    var inline354 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline353)
    _goml_runtime_core_string_println(inline354)
    test_int_comparisons()
    var inline349 string = ""
    var inline350 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline349)
    _goml_runtime_core_string_println(inline350)
    var inline345 string = "=== Float Comparisons ==="
    var inline346 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline345)
    _goml_runtime_core_string_println(inline346)
    test_float_comparisons()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t245 string = _goml_runtime_core_bool_to_string(self__64)
    return t245
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
