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
    var retv66 string
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv66 = t67
    return retv66
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv69 string
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t71 string = "i32(" + t70
    var t72 string = t71 + ")"
    retv69 = t72
    return retv69
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv74 bool
    var t75 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__2, other__3)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv77 int32
    var t78 int32 = self__4 * 16777619
    var t79 int32 = t78 + 216613626
    retv77 = t79
    return retv77
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv81 int32
    var t82 int32 = self__5 + other__6
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv84 int32
    var t85 int32 = self__7 * factor__8
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv87 string
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t89 string = "<" + t88
    var t90 string = t89 + ">"
    retv87 = t90
    return retv87
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv92 string
    var t93 int32 = self__10.value
    var t94 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t93)
    var t95 string = "Boxed(" + t94
    var t96 string = t95 + ")"
    retv92 = t96
    return retv92
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv98 string
    var t99 int32 = self__11.value
    var t100 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t99)
    var t101 string = "Boxed{value=" + t100
    var t102 string = t101 + "}"
    retv98 = t102
    return retv98
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv104 bool
    var t105 int32 = self__12.value
    var t106 int32 = other__13.value
    var t107 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t105, t106)
    retv104 = t107
    return retv104
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv109 int32
    var t110 int32 = self__14.value
    var t111 int32 = t110 * 31
    var t112 int32 = t111 + 7
    var t113 int32 = t112 * 1315423911
    retv109 = t113
    return retv109
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv115 Boxed
    var t116 int32 = self__15.value
    var t117 int32 = other__16.value
    var t118 int32 = t116 + t117
    var t119 Boxed = Boxed{
        value: t118,
    }
    retv115 = t119
    return retv115
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv121 Boxed
    var t122 int32 = self__17.value
    var t123 int32 = t122 * factor__18
    var t124 Boxed = Boxed{
        value: t123,
    }
    retv121 = t124
    return retv121
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv126 string
    var t127 int32 = self__19.value
    var t128 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t127)
    var t129 string = "[" + t128
    var t130 string = t129 + "]"
    retv126 = t130
    return retv126
}

func bool_text(x__20 bool) string {
    var retv132 string
    var jp134 string
    if x__20 {
        jp134 = "true"
    } else {
        jp134 = "false"
    }
    retv132 = jp134
    return retv132
}

func main0() struct{} {
    var t136 string = full_report__Q_int32__T_int32(7, 10, 32)
    println__T_string(t136)
    var t137 Boxed = Boxed{
        value: 99,
    }
    var t138 Boxed = Boxed{
        value: 3,
    }
    var t139 Boxed = Boxed{
        value: 4,
    }
    var t140 string = full_report__Q_Boxed__T_Boxed(t137, t138, t139)
    println__T_string(t140)
    var t141 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    println__T_string(t141)
    var t142 Boxed = Boxed{
        value: 1,
    }
    var t143 Boxed = Boxed{
        value: 5,
    }
    var t144 Boxed = Boxed{
        value: 6,
    }
    var t145 Boxed = Boxed{
        value: 7,
    }
    var t146 string = sum_and_tag__Q_Boxed__T_Boxed(t142, t143, t144, t145)
    println__T_string(t146)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv148 string
    var t149 string = _goml_runtime_core_int32_to_string(self__5)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv151 bool
    var t152 bool = self__61 == other__62
    retv151 = t152
    return retv151
}

func println__T_string(value__1 string) struct{} {
    var t154 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv157 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t158 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv157 = t158
    return retv157
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv160 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t161 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv160 = t161
    return retv160
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv163 string
    var t164 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t164, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t165 string = header__43 + " "
    var t166 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t167 string = t165 + t166
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t169 string = " @" + t168
    var t170 string = t167 + t169
    retv163 = t170
    return retv163
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv172 string
    var t173 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t173, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t174 string = header__43 + " "
    var t175 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t176 string = t174 + t175
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t178 string = " @" + t177
    var t179 string = t176 + t178
    retv172 = t179
    return retv172
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv181 string
    retv181 = self__37
    return retv181
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv183 int32
    var t184 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t185 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t184, factor__25)
    retv183 = t185
    return retv183
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv187 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t188 string = header__31 + " "
    var t189 string = t188 + repr__32
    var t190 string = bool_text(same__30)
    var t191 string = " | eq=" + t190
    var t192 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t193 string = " | hash=" + t192
    var t194 string = t191 + t193
    var t195 string = t189 + t194
    retv187 = t195
    return retv187
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv197 Boxed
    var t198 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t199 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t198, factor__25)
    retv197 = t199
    return retv197
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv201 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t202 string = header__31 + " "
    var t203 string = t202 + repr__32
    var t204 string = bool_text(same__30)
    var t205 string = " | eq=" + t204
    var t206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t207 string = " | hash=" + t206
    var t208 string = t205 + t207
    var t209 string = t203 + t208
    retv201 = t209
    return retv201
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv211 string
    var t212 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t213 string = t212 + "#"
    var t214 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t214)
    var t216 string = t213 + t215
    retv211 = t216
    return retv211
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv218 string
    var t219 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t220 string = t219 + "#"
    var t221 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t221)
    var t223 string = t220 + t222
    retv218 = t223
    return retv218
}

func show_both__T_int32(x__21 int32) string {
    var retv225 string
    var t226 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t227 string = t226 + " / "
    var t228 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t229 string = t227 + t228
    retv225 = t229
    return retv225
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv231 string
    var t232 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t233 string = t232 + " / "
    var t234 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t235 string = t233 + t234
    retv231 = t235
    return retv231
}

func main() {
    main0()
}
