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
    var retv157 string
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv157 = t158
    return retv157
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv160 string
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t162 string = "i32(" + t161
    var t163 string = t162 + ")"
    retv160 = t163
    return retv160
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv165 bool
    var t166 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__2, other__3)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv168 int32
    var t169 int32 = self__4 * 16777619
    var t170 int32 = t169 + 216613626
    retv168 = t170
    return retv168
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv172 int32
    var t173 int32 = self__5 + other__6
    retv172 = t173
    return retv172
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv175 int32
    var t176 int32 = self__7 * factor__8
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv178 string
    var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t180 string = "<" + t179
    var t181 string = t180 + ">"
    retv178 = t181
    return retv178
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv183 string
    var t184 int32 = self__10.value
    var t185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t184)
    var t186 string = "Boxed(" + t185
    var t187 string = t186 + ")"
    retv183 = t187
    return retv183
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv189 string
    var t190 int32 = self__11.value
    var t191 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t190)
    var t192 string = "Boxed{value=" + t191
    var t193 string = t192 + "}"
    retv189 = t193
    return retv189
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv195 bool
    var t196 int32 = self__12.value
    var t197 int32 = other__13.value
    var t198 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t196, t197)
    retv195 = t198
    return retv195
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv200 int32
    var t201 int32 = self__14.value
    var t202 int32 = t201 * 31
    var t203 int32 = t202 + 7
    var t204 int32 = t203 * 1315423911
    retv200 = t204
    return retv200
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv206 Boxed
    var t207 int32 = self__15.value
    var t208 int32 = other__16.value
    var t209 int32 = t207 + t208
    var t210 Boxed = Boxed{
        value: t209,
    }
    retv206 = t210
    return retv206
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv212 Boxed
    var t213 int32 = self__17.value
    var t214 int32 = t213 * factor__18
    var t215 Boxed = Boxed{
        value: t214,
    }
    retv212 = t215
    return retv212
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv217 string
    var t218 int32 = self__19.value
    var t219 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t218)
    var t220 string = "[" + t219
    var t221 string = t220 + "]"
    retv217 = t221
    return retv217
}

func bool_text(x__20 bool) string {
    var retv223 string
    var jp225 string
    if x__20 {
        jp225 = "true"
    } else {
        jp225 = "false"
    }
    retv223 = jp225
    return retv223
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t227 string = full_report__Q_int32__T_int32(tag__45, left__46, right__47)
    println__T_string(t227)
    var t228 Boxed = Boxed{
        value: 99,
    }
    var t229 Boxed = Boxed{
        value: 3,
    }
    var t230 Boxed = Boxed{
        value: 4,
    }
    var t231 string = full_report__Q_Boxed__T_Boxed(t228, t229, t230)
    println__T_string(t231)
    var t232 string = sum_and_tag__Q_int32__T_int32(sum_tag__48, first__49, second__50, third__51)
    println__T_string(t232)
    var t233 Boxed = Boxed{
        value: 1,
    }
    var t234 Boxed = Boxed{
        value: 5,
    }
    var t235 Boxed = Boxed{
        value: 6,
    }
    var t236 Boxed = Boxed{
        value: 7,
    }
    var t237 string = sum_and_tag__Q_Boxed__T_Boxed(t233, t234, t235, t236)
    println__T_string(t237)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv239 string
    var t240 string = _goml_runtime_core_int32_to_string(self__6)
    retv239 = t240
    return retv239
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv242 bool
    var t243 bool = self__65 == other__66
    retv242 = t243
    return retv242
}

func println__T_string(value__1 string) struct{} {
    var t245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t245)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv248 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t249 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv248 = t249
    return retv248
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv251 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t252 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv251 = t252
    return retv251
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv254 string
    var t255 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t255, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t256 string = header__43 + " "
    var t257 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t258 string = t256 + t257
    var t259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t260 string = " @" + t259
    var t261 string = t258 + t260
    retv254 = t261
    return retv254
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv263 string
    var t264 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t264, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t265 string = header__43 + " "
    var t266 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t267 string = t265 + t266
    var t268 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t269 string = " @" + t268
    var t270 string = t267 + t269
    retv263 = t270
    return retv263
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv272 string
    retv272 = self__38
    return retv272
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv274 int32
    var t275 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t276 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t275, factor__25)
    retv274 = t276
    return retv274
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv278 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t279 string = header__31 + " "
    var t280 string = t279 + repr__32
    var t281 string = bool_text(same__30)
    var t282 string = " | eq=" + t281
    var t283 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t284 string = " | hash=" + t283
    var t285 string = t282 + t284
    var t286 string = t280 + t285
    retv278 = t286
    return retv278
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv288 Boxed
    var t289 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t290 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t289, factor__25)
    retv288 = t290
    return retv288
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv292 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t293 string = header__31 + " "
    var t294 string = t293 + repr__32
    var t295 string = bool_text(same__30)
    var t296 string = " | eq=" + t295
    var t297 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t298 string = " | hash=" + t297
    var t299 string = t296 + t298
    var t300 string = t294 + t299
    retv292 = t300
    return retv292
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv302 string
    var t303 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t304 string = t303 + "#"
    var t305 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t306 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t305)
    var t307 string = t304 + t306
    retv302 = t307
    return retv302
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv309 string
    var t310 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t311 string = t310 + "#"
    var t312 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t313 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t312)
    var t314 string = t311 + t313
    retv309 = t314
    return retv309
}

func show_both__T_int32(x__21 int32) string {
    var retv316 string
    var t317 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t318 string = t317 + " / "
    var t319 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t320 string = t318 + t319
    retv316 = t320
    return retv316
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv322 string
    var t323 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t324 string = t323 + " / "
    var t325 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t326 string = t324 + t325
    retv322 = t326
    return retv322
}

func main() {
    main0()
}
