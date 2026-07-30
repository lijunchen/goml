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
    var retv73 string
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv76 string
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t78 string = "i32(" + t77
    var t79 string = t78 + ")"
    retv76 = t79
    return retv76
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv81 bool
    var t82 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__2, other__3)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv84 int32
    var t85 int32 = self__4 * 16777619
    var t86 int32 = t85 + 216613626
    retv84 = t86
    return retv84
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv88 int32
    var t89 int32 = self__5 + other__6
    retv88 = t89
    return retv88
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv91 int32
    var t92 int32 = self__7 * factor__8
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv94 string
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t96 string = "<" + t95
    var t97 string = t96 + ">"
    retv94 = t97
    return retv94
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv99 string
    var t100 int32 = self__10.value
    var t101 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t100)
    var t102 string = "Boxed(" + t101
    var t103 string = t102 + ")"
    retv99 = t103
    return retv99
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv105 string
    var t106 int32 = self__11.value
    var t107 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t106)
    var t108 string = "Boxed{value=" + t107
    var t109 string = t108 + "}"
    retv105 = t109
    return retv105
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv111 bool
    var t112 int32 = self__12.value
    var t113 int32 = other__13.value
    var t114 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t112, t113)
    retv111 = t114
    return retv111
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv116 int32
    var t117 int32 = self__14.value
    var t118 int32 = t117 * 31
    var t119 int32 = t118 + 7
    var t120 int32 = t119 * 1315423911
    retv116 = t120
    return retv116
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv122 Boxed
    var t123 int32 = self__15.value
    var t124 int32 = other__16.value
    var t125 int32 = t123 + t124
    var t126 Boxed = Boxed{
        value: t125,
    }
    retv122 = t126
    return retv122
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv128 Boxed
    var t129 int32 = self__17.value
    var t130 int32 = t129 * factor__18
    var t131 Boxed = Boxed{
        value: t130,
    }
    retv128 = t131
    return retv128
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv133 string
    var t134 int32 = self__19.value
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t134)
    var t136 string = "[" + t135
    var t137 string = t136 + "]"
    retv133 = t137
    return retv133
}

func bool_text(x__20 bool) string {
    var retv139 string
    var jp141 string
    if x__20 {
        jp141 = "true"
    } else {
        jp141 = "false"
    }
    retv139 = jp141
    return retv139
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t143 string = full_report__Q_int32__T_int32(tag__45, left__46, right__47)
    println__T_string(t143)
    var t144 Boxed = Boxed{
        value: 99,
    }
    var t145 Boxed = Boxed{
        value: 3,
    }
    var t146 Boxed = Boxed{
        value: 4,
    }
    var t147 string = full_report__Q_Boxed__T_Boxed(t144, t145, t146)
    println__T_string(t147)
    var t148 string = sum_and_tag__Q_int32__T_int32(sum_tag__48, first__49, second__50, third__51)
    println__T_string(t148)
    var t149 Boxed = Boxed{
        value: 1,
    }
    var t150 Boxed = Boxed{
        value: 5,
    }
    var t151 Boxed = Boxed{
        value: 6,
    }
    var t152 Boxed = Boxed{
        value: 7,
    }
    var t153 string = sum_and_tag__Q_Boxed__T_Boxed(t149, t150, t151, t152)
    println__T_string(t153)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv155 string
    var t156 string = _goml_runtime_core_int32_to_string(self__6)
    retv155 = t156
    return retv155
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv158 bool
    var t159 bool = self__65 == other__66
    retv158 = t159
    return retv158
}

func println__T_string(value__1 string) struct{} {
    var t161 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv164 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t165 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv164 = t165
    return retv164
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv167 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t168 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv167 = t168
    return retv167
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv170 string
    var t171 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t171, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t172 string = header__43 + " "
    var t173 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t174 string = t172 + t173
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t176 string = " @" + t175
    var t177 string = t174 + t176
    retv170 = t177
    return retv170
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv179 string
    var t180 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t180, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t181 string = header__43 + " "
    var t182 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t183 string = t181 + t182
    var t184 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t185 string = " @" + t184
    var t186 string = t183 + t185
    retv179 = t186
    return retv179
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv188 string
    retv188 = self__38
    return retv188
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv190 int32
    var t191 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t192 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t191, factor__25)
    retv190 = t192
    return retv190
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv194 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t195 string = header__31 + " "
    var t196 string = t195 + repr__32
    var t197 string = bool_text(same__30)
    var t198 string = " | eq=" + t197
    var t199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t200 string = " | hash=" + t199
    var t201 string = t198 + t200
    var t202 string = t196 + t201
    retv194 = t202
    return retv194
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv204 Boxed
    var t205 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t206 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t205, factor__25)
    retv204 = t206
    return retv204
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv208 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t209 string = header__31 + " "
    var t210 string = t209 + repr__32
    var t211 string = bool_text(same__30)
    var t212 string = " | eq=" + t211
    var t213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t214 string = " | hash=" + t213
    var t215 string = t212 + t214
    var t216 string = t210 + t215
    retv208 = t216
    return retv208
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv218 string
    var t219 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t220 string = t219 + "#"
    var t221 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t221)
    var t223 string = t220 + t222
    retv218 = t223
    return retv218
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv225 string
    var t226 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t227 string = t226 + "#"
    var t228 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t229 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t228)
    var t230 string = t227 + t229
    retv225 = t230
    return retv225
}

func show_both__T_int32(x__21 int32) string {
    var retv232 string
    var t233 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t234 string = t233 + " / "
    var t235 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t236 string = t234 + t235
    retv232 = t236
    return retv232
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv238 string
    var t239 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t240 string = t239 + " / "
    var t241 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t242 string = t240 + t241
    retv238 = t242
    return retv238
}

func main() {
    main0()
}
