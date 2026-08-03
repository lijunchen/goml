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
    var t162 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t163 string = "10 < 20: " + t162
    println__T_string(t163)
    var greater__4 bool = b__1 > a__0
    var t164 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t165 string = "20 > 10: " + t164
    println__T_string(t165)
    var less_eq1__5 bool = a__0 <= b__1
    var t166 string
    var inline269 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t166 = inline269
    var t167 string = "10 <= 20: " + t166
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline266)
    var less_eq2__6 bool = a__0 <= c__2
    var t168 string
    var inline264 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t168 = inline264
    var t169 string = "10 <= 10: " + t168
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline261)
    var greater_eq1__7 bool = b__1 >= a__0
    var t170 string
    var inline259 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t170 = inline259
    var t171 string = "20 >= 10: " + t170
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline256)
    var greater_eq2__8 bool = c__2 >= a__0
    var t172 string
    var inline254 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t172 = inline254
    var t173 string = "10 >= 10: " + t172
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline251)
    var eq1__9 bool
    var inline249 bool = a__0 == c__2
    eq1__9 = inline249
    var t174 string
    var inline247 string = _goml_runtime_core_bool_to_string(eq1__9)
    t174 = inline247
    var t175 string = "10 == 10: " + t174
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline244)
    var eq2__10 bool
    var inline242 bool = a__0 == b__1
    eq2__10 = inline242
    var t176 string
    var inline240 string = _goml_runtime_core_bool_to_string(eq2__10)
    t176 = inline240
    var t177 string = "10 == 20: " + t176
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline237)
    var t178 bool
    var inline235 bool = a__0 == b__1
    t178 = inline235
    var neq1__11 bool = !t178
    var t179 string
    var inline233 string = _goml_runtime_core_bool_to_string(neq1__11)
    t179 = inline233
    var t180 string = "10 != 20: " + t179
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline230)
    var t181 bool
    var inline228 bool = a__0 == c__2
    t181 = inline228
    var neq2__12 bool = !t181
    var t182 string
    var inline226 string = _goml_runtime_core_bool_to_string(neq2__12)
    t182 = inline226
    var t183 string = "10 != 10: " + t182
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t185 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t186 string = "2.71 < 3.14: " + t185
    println__T_string(t186)
    var greater__17 bool = x__13 > y__14
    var t187 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t188 string = "3.14 > 2.71: " + t187
    println__T_string(t188)
    var less_eq1__18 bool = y__14 <= x__13
    var t189 string
    var inline317 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t189 = inline317
    var t190 string = "2.71 <= 3.14: " + t189
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline314)
    var less_eq2__19 bool = x__13 <= z__15
    var t191 string
    var inline312 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t191 = inline312
    var t192 string = "3.14 <= 3.14: " + t191
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline309)
    var greater_eq1__20 bool = x__13 >= y__14
    var t193 string
    var inline307 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t193 = inline307
    var t194 string = "3.14 >= 2.71: " + t193
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline304)
    var greater_eq2__21 bool = z__15 >= x__13
    var t195 string
    var inline302 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t195 = inline302
    var t196 string = "3.14 >= 3.14: " + t195
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline299)
    var eq1__22 bool
    var inline297 bool = x__13 == z__15
    eq1__22 = inline297
    var t197 string
    var inline295 string = _goml_runtime_core_bool_to_string(eq1__22)
    t197 = inline295
    var t198 string = "3.14 == 3.14: " + t197
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline292)
    var eq2__23 bool
    var inline290 bool = x__13 == y__14
    eq2__23 = inline290
    var t199 string
    var inline288 string = _goml_runtime_core_bool_to_string(eq2__23)
    t199 = inline288
    var t200 string = "3.14 == 2.71: " + t199
    var inline285 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline285)
    var t201 bool
    var inline283 bool = x__13 == y__14
    t201 = inline283
    var neq1__24 bool = !t201
    var t202 string
    var inline281 string = _goml_runtime_core_bool_to_string(neq1__24)
    t202 = inline281
    var t203 string = "3.14 != 2.71: " + t202
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline278)
    var t204 bool
    var inline276 bool = x__13 == z__15
    t204 = inline276
    var neq2__25 bool = !t204
    var t205 string
    var inline274 string = _goml_runtime_core_bool_to_string(neq2__25)
    t205 = inline274
    var t206 string = "3.14 != 3.14: " + t205
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline271)
    return struct{}{}
}

func main0() struct{} {
    var inline327 string = "=== Integer Comparisons ==="
    var inline328 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline327)
    _goml_runtime_core_string_println(inline328)
    test_int_comparisons()
    var inline323 string = ""
    var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline323)
    _goml_runtime_core_string_println(inline324)
    var inline319 string = "=== Float Comparisons ==="
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline319)
    _goml_runtime_core_string_println(inline320)
    test_float_comparisons()
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t209 string
    t209 = value__31
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t213 string = _goml_runtime_core_bool_to_string(self__66)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
