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
    var retv69 string
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv69 = t70
    return retv69
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv72 string
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t74 string = "i32(" + t73
    var t75 string = t74 + ")"
    retv72 = t75
    return retv72
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv77 bool
    var t78 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__2, other__3)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv80 int32
    var t81 int32 = self__4 * 16777619
    var t82 int32 = t81 + 216613626
    retv80 = t82
    return retv80
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv84 int32
    var t85 int32 = self__5 + other__6
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv87 int32
    var t88 int32 = self__7 * factor__8
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv90 string
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t92 string = "<" + t91
    var t93 string = t92 + ">"
    retv90 = t93
    return retv90
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv95 string
    var t96 int32 = self__10.value
    var t97 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t96)
    var t98 string = "Boxed(" + t97
    var t99 string = t98 + ")"
    retv95 = t99
    return retv95
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv101 string
    var t102 int32 = self__11.value
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t102)
    var t104 string = "Boxed{value=" + t103
    var t105 string = t104 + "}"
    retv101 = t105
    return retv101
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv107 bool
    var t108 int32 = self__12.value
    var t109 int32 = other__13.value
    var t110 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t108, t109)
    retv107 = t110
    return retv107
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv112 int32
    var t113 int32 = self__14.value
    var t114 int32 = t113 * 31
    var t115 int32 = t114 + 7
    var t116 int32 = t115 * 1315423911
    retv112 = t116
    return retv112
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv118 Boxed
    var t119 int32 = self__15.value
    var t120 int32 = other__16.value
    var t121 int32 = t119 + t120
    var t122 Boxed = Boxed{
        value: t121,
    }
    retv118 = t122
    return retv118
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv124 Boxed
    var t125 int32 = self__17.value
    var t126 int32 = t125 * factor__18
    var t127 Boxed = Boxed{
        value: t126,
    }
    retv124 = t127
    return retv124
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv129 string
    var t130 int32 = self__19.value
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t130)
    var t132 string = "[" + t131
    var t133 string = t132 + "]"
    retv129 = t133
    return retv129
}

func bool_text(x__20 bool) string {
    var retv135 string
    var jp137 string
    if x__20 {
        jp137 = "true"
    } else {
        jp137 = "false"
    }
    retv135 = jp137
    return retv135
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t139 string = full_report__Q_int32__T_int32(tag__45, left__46, right__47)
    println__T_string(t139)
    var t140 Boxed = Boxed{
        value: 99,
    }
    var t141 Boxed = Boxed{
        value: 3,
    }
    var t142 Boxed = Boxed{
        value: 4,
    }
    var t143 string = full_report__Q_Boxed__T_Boxed(t140, t141, t142)
    println__T_string(t143)
    var t144 string = sum_and_tag__Q_int32__T_int32(sum_tag__48, first__49, second__50, third__51)
    println__T_string(t144)
    var t145 Boxed = Boxed{
        value: 1,
    }
    var t146 Boxed = Boxed{
        value: 5,
    }
    var t147 Boxed = Boxed{
        value: 6,
    }
    var t148 Boxed = Boxed{
        value: 7,
    }
    var t149 string = sum_and_tag__Q_Boxed__T_Boxed(t145, t146, t147, t148)
    println__T_string(t149)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int32_to_string(self__6)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv154 bool
    var t155 bool = self__65 == other__66
    retv154 = t155
    return retv154
}

func println__T_string(value__1 string) struct{} {
    var t157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t157)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv160 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t161 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv160 = t161
    return retv160
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv163 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t164 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv163 = t164
    return retv163
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv166 string
    var t167 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t167, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t168 string = header__43 + " "
    var t169 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t170 string = t168 + t169
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t172 string = " @" + t171
    var t173 string = t170 + t172
    retv166 = t173
    return retv166
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv175 string
    var t176 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t176, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t177 string = header__43 + " "
    var t178 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t179 string = t177 + t178
    var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t181 string = " @" + t180
    var t182 string = t179 + t181
    retv175 = t182
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv186 int32
    var t187 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t188 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t187, factor__25)
    retv186 = t188
    return retv186
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv190 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t191 string = header__31 + " "
    var t192 string = t191 + repr__32
    var t193 string = bool_text(same__30)
    var t194 string = " | eq=" + t193
    var t195 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t196 string = " | hash=" + t195
    var t197 string = t194 + t196
    var t198 string = t192 + t197
    retv190 = t198
    return retv190
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv200 Boxed
    var t201 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t202 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t201, factor__25)
    retv200 = t202
    return retv200
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv204 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t205 string = header__31 + " "
    var t206 string = t205 + repr__32
    var t207 string = bool_text(same__30)
    var t208 string = " | eq=" + t207
    var t209 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t210 string = " | hash=" + t209
    var t211 string = t208 + t210
    var t212 string = t206 + t211
    retv204 = t212
    return retv204
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv214 string
    var t215 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t216 string = t215 + "#"
    var t217 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t217)
    var t219 string = t216 + t218
    retv214 = t219
    return retv214
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv221 string
    var t222 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t223 string = t222 + "#"
    var t224 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t224)
    var t226 string = t223 + t225
    retv221 = t226
    return retv221
}

func show_both__T_int32(x__21 int32) string {
    var retv228 string
    var t229 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t230 string = t229 + " / "
    var t231 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t232 string = t230 + t231
    retv228 = t232
    return retv228
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv234 string
    var t235 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t236 string = t235 + " / "
    var t237 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t238 string = t236 + t237
    retv234 = t238
    return retv234
}

func main() {
    main0()
}
