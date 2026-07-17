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
    var retv63 string
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv63 = t64
    return retv63
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv66 string
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t68 string = "i32(" + t67
    var t69 string = t68 + ")"
    retv66 = t69
    return retv66
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv71 bool
    var t72 bool = self__2 == other__3
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv74 int32
    var t75 int32 = self__4 * 16777619
    var t76 int32 = t75 + 216613626
    retv74 = t76
    return retv74
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv78 int32
    var t79 int32 = self__5 + other__6
    retv78 = t79
    return retv78
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv81 int32
    var t82 int32 = self__7 * factor__8
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv84 string
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t86 string = "<" + t85
    var t87 string = t86 + ">"
    retv84 = t87
    return retv84
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv89 string
    var t90 int32 = self__10.value
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    var t92 string = "Boxed(" + t91
    var t93 string = t92 + ")"
    retv89 = t93
    return retv89
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv95 string
    var t96 int32 = self__11.value
    var t97 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t96)
    var t98 string = "Boxed{value=" + t97
    var t99 string = t98 + "}"
    retv95 = t99
    return retv95
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv101 bool
    var t102 int32 = self__12.value
    var t103 int32 = other__13.value
    var t104 bool = t102 == t103
    retv101 = t104
    return retv101
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv106 int32
    var t107 int32 = self__14.value
    var t108 int32 = t107 * 31
    var t109 int32 = t108 + 7
    var t110 int32 = t109 * 1315423911
    retv106 = t110
    return retv106
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv112 Boxed
    var t113 int32 = self__15.value
    var t114 int32 = other__16.value
    var t115 int32 = t113 + t114
    var t116 Boxed = Boxed{
        value: t115,
    }
    retv112 = t116
    return retv112
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv118 Boxed
    var t119 int32 = self__17.value
    var t120 int32 = t119 * factor__18
    var t121 Boxed = Boxed{
        value: t120,
    }
    retv118 = t121
    return retv118
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv123 string
    var t124 int32 = self__19.value
    var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t124)
    var t126 string = "[" + t125
    var t127 string = t126 + "]"
    retv123 = t127
    return retv123
}

func bool_text(x__20 bool) string {
    var retv129 string
    var jp131 string
    if x__20 {
        jp131 = "true"
    } else {
        jp131 = "false"
    }
    retv129 = jp131
    return retv129
}

func main0() struct{} {
    var t133 string = full_report__Q_int32__T_int32(7, 10, 32)
    println__T_string(t133)
    var t134 Boxed = Boxed{
        value: 99,
    }
    var t135 Boxed = Boxed{
        value: 3,
    }
    var t136 Boxed = Boxed{
        value: 4,
    }
    var t137 string = full_report__Q_Boxed__T_Boxed(t134, t135, t136)
    println__T_string(t137)
    var t138 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    println__T_string(t138)
    var t139 Boxed = Boxed{
        value: 1,
    }
    var t140 Boxed = Boxed{
        value: 5,
    }
    var t141 Boxed = Boxed{
        value: 6,
    }
    var t142 Boxed = Boxed{
        value: 7,
    }
    var t143 string = sum_and_tag__Q_Boxed__T_Boxed(t139, t140, t141, t142)
    println__T_string(t143)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv145 string
    var t146 string = _goml_runtime_core_int32_to_string(self__2)
    retv145 = t146
    return retv145
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv151 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t152 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv151 = t152
    return retv151
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv154 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t155 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv154 = t155
    return retv154
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv157 string
    var t158 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t158, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t159 string = header__43 + " "
    var t160 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t161 string = t159 + t160
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t163 string = " @" + t162
    var t164 string = t161 + t163
    retv157 = t164
    return retv157
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv166 string
    var t167 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t167, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t168 string = header__43 + " "
    var t169 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t170 string = t168 + t169
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t172 string = " @" + t171
    var t173 string = t170 + t172
    retv166 = t173
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv175 string
    retv175 = self__34
    return retv175
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv177 int32
    var t178 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t179 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t178, factor__25)
    retv177 = t179
    return retv177
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv181 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t182 string = header__31 + " "
    var t183 string = t182 + repr__32
    var t184 string = bool_text(same__30)
    var t185 string = " | eq=" + t184
    var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t187 string = " | hash=" + t186
    var t188 string = t185 + t187
    var t189 string = t183 + t188
    retv181 = t189
    return retv181
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv191 Boxed
    var t192 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t193 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t192, factor__25)
    retv191 = t193
    return retv191
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv195 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t196 string = header__31 + " "
    var t197 string = t196 + repr__32
    var t198 string = bool_text(same__30)
    var t199 string = " | eq=" + t198
    var t200 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t201 string = " | hash=" + t200
    var t202 string = t199 + t201
    var t203 string = t197 + t202
    retv195 = t203
    return retv195
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv205 string
    var t206 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t207 string = t206 + "#"
    var t208 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t209 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t208)
    var t210 string = t207 + t209
    retv205 = t210
    return retv205
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv212 string
    var t213 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t214 string = t213 + "#"
    var t215 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t215)
    var t217 string = t214 + t216
    retv212 = t217
    return retv212
}

func show_both__T_int32(x__21 int32) string {
    var retv219 string
    var t220 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t221 string = t220 + " / "
    var t222 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t223 string = t221 + t222
    retv219 = t223
    return retv219
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv225 string
    var t226 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t227 string = t226 + " / "
    var t228 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t229 string = t227 + t228
    retv225 = t229
    return retv225
}

func main() {
    main0()
}
