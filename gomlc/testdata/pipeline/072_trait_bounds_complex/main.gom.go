package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Boxed struct {
    value int32
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var retv160 string
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv160 = t161
    return retv160
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv163 string
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t165 string = "i32(" + t164
    var t166 string = t165 + ")"
    retv163 = t166
    return retv163
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv168 bool
    var t169 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__2, other__3)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv171 int32
    var t172 int32 = self__4 * 16777619
    var t173 int32 = t172 + 216613626
    retv171 = t173
    return retv171
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv175 int32
    var t176 int32 = self__5 + other__6
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv178 int32
    var t179 int32 = self__7 * factor__8
    retv178 = t179
    return retv178
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv181 string
    var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t183 string = "<" + t182
    var t184 string = t183 + ">"
    retv181 = t184
    return retv181
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv186 string
    var t187 int32 = self__10.value
    var t188 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t187)
    var t189 string = "Boxed(" + t188
    var t190 string = t189 + ")"
    retv186 = t190
    return retv186
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv192 string
    var t193 int32 = self__11.value
    var t194 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t193)
    var t195 string = "Boxed{value=" + t194
    var t196 string = t195 + "}"
    retv192 = t196
    return retv192
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv198 bool
    var t199 int32 = self__12.value
    var t200 int32 = other__13.value
    var t201 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t199, t200)
    retv198 = t201
    return retv198
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv203 int32
    var t204 int32 = self__14.value
    var t205 int32 = t204 * 31
    var t206 int32 = t205 + 7
    var t207 int32 = t206 * 1315423911
    retv203 = t207
    return retv203
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv209 Boxed
    var t210 int32 = self__15.value
    var t211 int32 = other__16.value
    var t212 int32 = t210 + t211
    var t213 Boxed = Boxed{
        value: t212,
    }
    retv209 = t213
    return retv209
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv215 Boxed
    var t216 int32 = self__17.value
    var t217 int32 = t216 * factor__18
    var t218 Boxed = Boxed{
        value: t217,
    }
    retv215 = t218
    return retv215
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv220 string
    var t221 int32 = self__19.value
    var t222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t221)
    var t223 string = "[" + t222
    var t224 string = t223 + "]"
    retv220 = t224
    return retv220
}

func bool_text(x__20 bool) string {
    var retv226 string
    var jp228 string
    if x__20 {
        jp228 = "true"
    } else {
        jp228 = "false"
    }
    retv226 = jp228
    return retv226
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t230 string = full_report__Q_int32__T_int32(tag__45, left__46, right__47)
    println__T_string(t230)
    var t231 Boxed = Boxed{
        value: 99,
    }
    var t232 Boxed = Boxed{
        value: 3,
    }
    var t233 Boxed = Boxed{
        value: 4,
    }
    var t234 string = full_report__Q_Boxed__T_Boxed(t231, t232, t233)
    println__T_string(t234)
    var t235 string = sum_and_tag__Q_int32__T_int32(sum_tag__48, first__49, second__50, third__51)
    println__T_string(t235)
    var t236 Boxed = Boxed{
        value: 1,
    }
    var t237 Boxed = Boxed{
        value: 5,
    }
    var t238 Boxed = Boxed{
        value: 6,
    }
    var t239 Boxed = Boxed{
        value: 7,
    }
    var t240 string = sum_and_tag__Q_Boxed__T_Boxed(t236, t237, t238, t239)
    println__T_string(t240)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv242 string
    var t243 string = _goml_runtime_core_int32_to_string(self__6)
    retv242 = t243
    return retv242
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv245 bool
    var t246 bool = self__65 == other__66
    retv245 = t246
    return retv245
}

func println__T_string(value__1 string) struct{} {
    var t248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t248)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv251 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t252 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv251 = t252
    return retv251
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv254 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t255 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv254 = t255
    return retv254
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv257 string
    var t258 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t258, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t259 string = header__43 + " "
    var t260 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t261 string = t259 + t260
    var t262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t263 string = " @" + t262
    var t264 string = t261 + t263
    retv257 = t264
    return retv257
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv266 string
    var t267 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t267, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t268 string = header__43 + " "
    var t269 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t270 string = t268 + t269
    var t271 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t272 string = " @" + t271
    var t273 string = t270 + t272
    retv266 = t273
    return retv266
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv275 string
    retv275 = self__38
    return retv275
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv277 int32
    var t278 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t279 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t278, factor__25)
    retv277 = t279
    return retv277
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv281 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t282 string = header__31 + " "
    var t283 string = t282 + repr__32
    var t284 string = bool_text(same__30)
    var t285 string = " | eq=" + t284
    var t286 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t287 string = " | hash=" + t286
    var t288 string = t285 + t287
    var t289 string = t283 + t288
    retv281 = t289
    return retv281
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv291 Boxed
    var t292 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t293 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t292, factor__25)
    retv291 = t293
    return retv291
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv295 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t296 string = header__31 + " "
    var t297 string = t296 + repr__32
    var t298 string = bool_text(same__30)
    var t299 string = " | eq=" + t298
    var t300 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t301 string = " | hash=" + t300
    var t302 string = t299 + t301
    var t303 string = t297 + t302
    retv295 = t303
    return retv295
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv305 string
    var t306 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t307 string = t306 + "#"
    var t308 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t309 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t308)
    var t310 string = t307 + t309
    retv305 = t310
    return retv305
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv312 string
    var t313 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t314 string = t313 + "#"
    var t315 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t316 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t315)
    var t317 string = t314 + t316
    retv312 = t317
    return retv312
}

func show_both__T_int32(x__21 int32) string {
    var retv319 string
    var t320 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t321 string = t320 + " / "
    var t322 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t323 string = t321 + t322
    retv319 = t323
    return retv319
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv325 string
    var t326 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t327 string = t326 + " / "
    var t328 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t329 string = t327 + t328
    retv325 = t329
    return retv325
}

func main() {
    main0()
}
