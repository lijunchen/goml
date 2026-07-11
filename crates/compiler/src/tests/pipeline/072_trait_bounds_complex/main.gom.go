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
    var retv12 string
    var t13 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv12 = t13
    return retv12
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv15 string
    var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t17 string = "i32(" + t16
    var t18 string = t17 + ")"
    retv15 = t18
    return retv15
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv20 bool
    var t21 bool = self__2 == other__3
    retv20 = t21
    return retv20
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv23 int32
    var t24 int32 = self__4 * 16777619
    var t25 int32 = t24 + 216613626
    retv23 = t25
    return retv23
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv27 int32
    var t28 int32 = self__5 + other__6
    retv27 = t28
    return retv27
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv30 int32
    var t31 int32 = self__7 * factor__8
    retv30 = t31
    return retv30
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv33 string
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t35 string = "<" + t34
    var t36 string = t35 + ">"
    retv33 = t36
    return retv33
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv38 string
    var t39 int32 = self__10.value
    var t40 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t39)
    var t41 string = "Boxed(" + t40
    var t42 string = t41 + ")"
    retv38 = t42
    return retv38
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv44 string
    var t45 int32 = self__11.value
    var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t45)
    var t47 string = "Boxed{value=" + t46
    var t48 string = t47 + "}"
    retv44 = t48
    return retv44
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv50 bool
    var t51 int32 = self__12.value
    var t52 int32 = other__13.value
    var t53 bool = t51 == t52
    retv50 = t53
    return retv50
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv55 int32
    var t56 int32 = self__14.value
    var t57 int32 = t56 * 31
    var t58 int32 = t57 + 7
    var t59 int32 = t58 * 1315423911
    retv55 = t59
    return retv55
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv61 Boxed
    var t62 int32 = self__15.value
    var t63 int32 = other__16.value
    var t64 int32 = t62 + t63
    var t65 Boxed = Boxed{
        value: t64,
    }
    retv61 = t65
    return retv61
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv67 Boxed
    var t68 int32 = self__17.value
    var t69 int32 = t68 * factor__18
    var t70 Boxed = Boxed{
        value: t69,
    }
    retv67 = t70
    return retv67
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv72 string
    var t73 int32 = self__19.value
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t73)
    var t75 string = "[" + t74
    var t76 string = t75 + "]"
    retv72 = t76
    return retv72
}

func bool_text(x__20 bool) string {
    var retv78 string
    var jp80 string
    if x__20 {
        jp80 = "true"
    } else {
        jp80 = "false"
    }
    retv78 = jp80
    return retv78
}

func main0() struct{} {
    var t82 string = full_report__Q_int32__T_int32(7, 10, 32)
    println__T_string(t82)
    var t83 Boxed = Boxed{
        value: 99,
    }
    var t84 Boxed = Boxed{
        value: 3,
    }
    var t85 Boxed = Boxed{
        value: 4,
    }
    var t86 string = full_report__Q_Boxed__T_Boxed(t83, t84, t85)
    println__T_string(t86)
    var t87 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    println__T_string(t87)
    var t88 Boxed = Boxed{
        value: 1,
    }
    var t89 Boxed = Boxed{
        value: 5,
    }
    var t90 Boxed = Boxed{
        value: 6,
    }
    var t91 Boxed = Boxed{
        value: 7,
    }
    var t92 string = sum_and_tag__Q_Boxed__T_Boxed(t88, t89, t90, t91)
    println__T_string(t92)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__2)
    retv94 = t95
    return retv94
}

func println__T_string(value__1 string) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv100 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t101 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv100 = t101
    return retv100
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv103 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t104 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv103 = t104
    return retv103
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv106 string
    var t107 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t107, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t108 string = header__43 + " "
    var t109 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t110 string = t108 + t109
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t112 string = " @" + t111
    var t113 string = t110 + t112
    retv106 = t113
    return retv106
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv115 string
    var t116 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t116, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t117 string = header__43 + " "
    var t118 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t119 string = t117 + t118
    var t120 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t121 string = " @" + t120
    var t122 string = t119 + t121
    retv115 = t122
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv124 string
    retv124 = self__9
    return retv124
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv126 int32
    var t127 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t128 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t127, factor__25)
    retv126 = t128
    return retv126
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv130 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t131 string = header__31 + " "
    var t132 string = t131 + repr__32
    var t133 string = bool_text(same__30)
    var t134 string = " | eq=" + t133
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t136 string = " | hash=" + t135
    var t137 string = t134 + t136
    var t138 string = t132 + t137
    retv130 = t138
    return retv130
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv140 Boxed
    var t141 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t142 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t141, factor__25)
    retv140 = t142
    return retv140
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv144 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t145 string = header__31 + " "
    var t146 string = t145 + repr__32
    var t147 string = bool_text(same__30)
    var t148 string = " | eq=" + t147
    var t149 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t150 string = " | hash=" + t149
    var t151 string = t148 + t150
    var t152 string = t146 + t151
    retv144 = t152
    return retv144
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv154 string
    var t155 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t156 string = t155 + "#"
    var t157 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t157)
    var t159 string = t156 + t158
    retv154 = t159
    return retv154
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv161 string
    var t162 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t163 string = t162 + "#"
    var t164 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t164)
    var t166 string = t163 + t165
    retv161 = t166
    return retv161
}

func show_both__T_int32(x__21 int32) string {
    var retv168 string
    var t169 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t170 string = t169 + " / "
    var t171 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t172 string = t170 + t171
    retv168 = t172
    return retv168
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv174 string
    var t175 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t176 string = t175 + " / "
    var t177 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t178 string = t176 + t177
    retv174 = t178
    return retv174
}

func main() {
    main0()
}
