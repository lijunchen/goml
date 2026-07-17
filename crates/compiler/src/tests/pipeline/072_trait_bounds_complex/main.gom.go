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
    var t75 bool = self__2 == other__3
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
    var t107 bool = t105 == t106
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

func println__T_string(value__1 string) struct{} {
    var t151 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t151)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv154 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t155 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv154 = t155
    return retv154
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv157 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t158 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv157 = t158
    return retv157
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv160 string
    var t161 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t161, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t162 string = header__43 + " "
    var t163 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t164 string = t162 + t163
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t166 string = " @" + t165
    var t167 string = t164 + t166
    retv160 = t167
    return retv160
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv169 string
    var t170 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t170, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t171 string = header__43 + " "
    var t172 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t173 string = t171 + t172
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t175 string = " @" + t174
    var t176 string = t173 + t175
    retv169 = t176
    return retv169
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv178 string
    retv178 = self__37
    return retv178
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv180 int32
    var t181 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t182 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t181, factor__25)
    retv180 = t182
    return retv180
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv184 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t185 string = header__31 + " "
    var t186 string = t185 + repr__32
    var t187 string = bool_text(same__30)
    var t188 string = " | eq=" + t187
    var t189 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t190 string = " | hash=" + t189
    var t191 string = t188 + t190
    var t192 string = t186 + t191
    retv184 = t192
    return retv184
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv194 Boxed
    var t195 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t196 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t195, factor__25)
    retv194 = t196
    return retv194
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv198 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t199 string = header__31 + " "
    var t200 string = t199 + repr__32
    var t201 string = bool_text(same__30)
    var t202 string = " | eq=" + t201
    var t203 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t204 string = " | hash=" + t203
    var t205 string = t202 + t204
    var t206 string = t200 + t205
    retv198 = t206
    return retv198
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv208 string
    var t209 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t210 string = t209 + "#"
    var t211 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t212 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t211)
    var t213 string = t210 + t212
    retv208 = t213
    return retv208
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv215 string
    var t216 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t217 string = t216 + "#"
    var t218 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t219 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t218)
    var t220 string = t217 + t219
    retv215 = t220
    return retv215
}

func show_both__T_int32(x__21 int32) string {
    var retv222 string
    var t223 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t224 string = t223 + " / "
    var t225 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t226 string = t224 + t225
    retv222 = t226
    return retv222
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv228 string
    var t229 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t230 string = t229 + " / "
    var t231 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t232 string = t230 + t231
    retv228 = t232
    return retv228
}

func main() {
    main0()
}
