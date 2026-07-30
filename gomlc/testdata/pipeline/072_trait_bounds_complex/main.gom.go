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
    var retv113 string
    var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv116 string
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t118 string = "i32(" + t117
    var t119 string = t118 + ")"
    retv116 = t119
    return retv116
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv121 bool
    var t122 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__2, other__3)
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv124 int32
    var t125 int32 = self__4 * 16777619
    var t126 int32 = t125 + 216613626
    retv124 = t126
    return retv124
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv128 int32
    var t129 int32 = self__5 + other__6
    retv128 = t129
    return retv128
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv131 int32
    var t132 int32 = self__7 * factor__8
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv134 string
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t136 string = "<" + t135
    var t137 string = t136 + ">"
    retv134 = t137
    return retv134
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv139 string
    var t140 int32 = self__10.value
    var t141 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t140)
    var t142 string = "Boxed(" + t141
    var t143 string = t142 + ")"
    retv139 = t143
    return retv139
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv145 string
    var t146 int32 = self__11.value
    var t147 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t146)
    var t148 string = "Boxed{value=" + t147
    var t149 string = t148 + "}"
    retv145 = t149
    return retv145
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv151 bool
    var t152 int32 = self__12.value
    var t153 int32 = other__13.value
    var t154 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t152, t153)
    retv151 = t154
    return retv151
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv156 int32
    var t157 int32 = self__14.value
    var t158 int32 = t157 * 31
    var t159 int32 = t158 + 7
    var t160 int32 = t159 * 1315423911
    retv156 = t160
    return retv156
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv162 Boxed
    var t163 int32 = self__15.value
    var t164 int32 = other__16.value
    var t165 int32 = t163 + t164
    var t166 Boxed = Boxed{
        value: t165,
    }
    retv162 = t166
    return retv162
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv168 Boxed
    var t169 int32 = self__17.value
    var t170 int32 = t169 * factor__18
    var t171 Boxed = Boxed{
        value: t170,
    }
    retv168 = t171
    return retv168
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv173 string
    var t174 int32 = self__19.value
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t174)
    var t176 string = "[" + t175
    var t177 string = t176 + "]"
    retv173 = t177
    return retv173
}

func bool_text(x__20 bool) string {
    var retv179 string
    var jp181 string
    if x__20 {
        jp181 = "true"
    } else {
        jp181 = "false"
    }
    retv179 = jp181
    return retv179
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t183 string = full_report__Q_int32__T_int32(tag__45, left__46, right__47)
    println__T_string(t183)
    var t184 Boxed = Boxed{
        value: 99,
    }
    var t185 Boxed = Boxed{
        value: 3,
    }
    var t186 Boxed = Boxed{
        value: 4,
    }
    var t187 string = full_report__Q_Boxed__T_Boxed(t184, t185, t186)
    println__T_string(t187)
    var t188 string = sum_and_tag__Q_int32__T_int32(sum_tag__48, first__49, second__50, third__51)
    println__T_string(t188)
    var t189 Boxed = Boxed{
        value: 1,
    }
    var t190 Boxed = Boxed{
        value: 5,
    }
    var t191 Boxed = Boxed{
        value: 6,
    }
    var t192 Boxed = Boxed{
        value: 7,
    }
    var t193 string = sum_and_tag__Q_Boxed__T_Boxed(t189, t190, t191, t192)
    println__T_string(t193)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int32_to_string(self__6)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv198 bool
    var t199 bool = self__65 == other__66
    retv198 = t199
    return retv198
}

func println__T_string(value__1 string) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv204 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t205 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv204 = t205
    return retv204
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv207 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t208 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv207 = t208
    return retv207
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv210 string
    var t211 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t211, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t212 string = header__43 + " "
    var t213 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t214 string = t212 + t213
    var t215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t216 string = " @" + t215
    var t217 string = t214 + t216
    retv210 = t217
    return retv210
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv219 string
    var t220 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t220, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t221 string = header__43 + " "
    var t222 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t223 string = t221 + t222
    var t224 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t225 string = " @" + t224
    var t226 string = t223 + t225
    retv219 = t226
    return retv219
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv228 string
    retv228 = self__38
    return retv228
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv230 int32
    var t231 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t232 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t231, factor__25)
    retv230 = t232
    return retv230
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv234 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t235 string = header__31 + " "
    var t236 string = t235 + repr__32
    var t237 string = bool_text(same__30)
    var t238 string = " | eq=" + t237
    var t239 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t240 string = " | hash=" + t239
    var t241 string = t238 + t240
    var t242 string = t236 + t241
    retv234 = t242
    return retv234
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv244 Boxed
    var t245 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t246 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t245, factor__25)
    retv244 = t246
    return retv244
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv248 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t249 string = header__31 + " "
    var t250 string = t249 + repr__32
    var t251 string = bool_text(same__30)
    var t252 string = " | eq=" + t251
    var t253 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t254 string = " | hash=" + t253
    var t255 string = t252 + t254
    var t256 string = t250 + t255
    retv248 = t256
    return retv248
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv258 string
    var t259 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t260 string = t259 + "#"
    var t261 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t261)
    var t263 string = t260 + t262
    retv258 = t263
    return retv258
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv265 string
    var t266 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t267 string = t266 + "#"
    var t268 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t268)
    var t270 string = t267 + t269
    retv265 = t270
    return retv265
}

func show_both__T_int32(x__21 int32) string {
    var retv272 string
    var t273 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t274 string = t273 + " / "
    var t275 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t276 string = t274 + t275
    retv272 = t276
    return retv272
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv278 string
    var t279 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t280 string = t279 + " / "
    var t281 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t282 string = t280 + t281
    retv278 = t282
    return retv278
}

func main() {
    main0()
}
