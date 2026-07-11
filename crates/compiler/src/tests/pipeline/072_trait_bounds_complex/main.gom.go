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
    var retv27 string
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv27 = t28
    return retv27
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv30 string
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t32 string = "i32(" + t31
    var t33 string = t32 + ")"
    retv30 = t33
    return retv30
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv35 bool
    var t36 bool = self__2 == other__3
    retv35 = t36
    return retv35
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv38 int32
    var t39 int32 = self__4 * 16777619
    var t40 int32 = t39 + 216613626
    retv38 = t40
    return retv38
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv42 int32
    var t43 int32 = self__5 + other__6
    retv42 = t43
    return retv42
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv45 int32
    var t46 int32 = self__7 * factor__8
    retv45 = t46
    return retv45
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv48 string
    var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t50 string = "<" + t49
    var t51 string = t50 + ">"
    retv48 = t51
    return retv48
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv53 string
    var t54 int32 = self__10.value
    var t55 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t54)
    var t56 string = "Boxed(" + t55
    var t57 string = t56 + ")"
    retv53 = t57
    return retv53
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv59 string
    var t60 int32 = self__11.value
    var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t60)
    var t62 string = "Boxed{value=" + t61
    var t63 string = t62 + "}"
    retv59 = t63
    return retv59
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv65 bool
    var t66 int32 = self__12.value
    var t67 int32 = other__13.value
    var t68 bool = t66 == t67
    retv65 = t68
    return retv65
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv70 int32
    var t71 int32 = self__14.value
    var t72 int32 = t71 * 31
    var t73 int32 = t72 + 7
    var t74 int32 = t73 * 1315423911
    retv70 = t74
    return retv70
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv76 Boxed
    var t77 int32 = self__15.value
    var t78 int32 = other__16.value
    var t79 int32 = t77 + t78
    var t80 Boxed = Boxed{
        value: t79,
    }
    retv76 = t80
    return retv76
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv82 Boxed
    var t83 int32 = self__17.value
    var t84 int32 = t83 * factor__18
    var t85 Boxed = Boxed{
        value: t84,
    }
    retv82 = t85
    return retv82
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv87 string
    var t88 int32 = self__19.value
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t88)
    var t90 string = "[" + t89
    var t91 string = t90 + "]"
    retv87 = t91
    return retv87
}

func bool_text(x__20 bool) string {
    var retv93 string
    var jp95 string
    if x__20 {
        jp95 = "true"
    } else {
        jp95 = "false"
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t97 string = full_report__Q_int32__T_int32(7, 10, 32)
    println__T_string(t97)
    var t98 Boxed = Boxed{
        value: 99,
    }
    var t99 Boxed = Boxed{
        value: 3,
    }
    var t100 Boxed = Boxed{
        value: 4,
    }
    var t101 string = full_report__Q_Boxed__T_Boxed(t98, t99, t100)
    println__T_string(t101)
    var t102 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    println__T_string(t102)
    var t103 Boxed = Boxed{
        value: 1,
    }
    var t104 Boxed = Boxed{
        value: 5,
    }
    var t105 Boxed = Boxed{
        value: 6,
    }
    var t106 Boxed = Boxed{
        value: 7,
    }
    var t107 string = sum_and_tag__Q_Boxed__T_Boxed(t103, t104, t105, t106)
    println__T_string(t107)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv109 string
    var t110 string = _goml_runtime_core_int32_to_string(self__2)
    retv109 = t110
    return retv109
}

func println__T_string(value__1 string) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv115 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t116 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv115 = t116
    return retv115
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv118 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t119 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv118 = t119
    return retv118
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv121 string
    var t122 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t122, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t123 string = header__43 + " "
    var t124 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t125 string = t123 + t124
    var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t127 string = " @" + t126
    var t128 string = t125 + t127
    retv121 = t128
    return retv121
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv130 string
    var t131 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t131, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t132 string = header__43 + " "
    var t133 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t134 string = t132 + t133
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t136 string = " @" + t135
    var t137 string = t134 + t136
    retv130 = t137
    return retv130
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv139 string
    retv139 = self__9
    return retv139
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv141 int32
    var t142 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t143 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t142, factor__25)
    retv141 = t143
    return retv141
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv145 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t146 string = header__31 + " "
    var t147 string = t146 + repr__32
    var t148 string = bool_text(same__30)
    var t149 string = " | eq=" + t148
    var t150 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t151 string = " | hash=" + t150
    var t152 string = t149 + t151
    var t153 string = t147 + t152
    retv145 = t153
    return retv145
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv155 Boxed
    var t156 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t157 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t156, factor__25)
    retv155 = t157
    return retv155
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv159 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t160 string = header__31 + " "
    var t161 string = t160 + repr__32
    var t162 string = bool_text(same__30)
    var t163 string = " | eq=" + t162
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t165 string = " | hash=" + t164
    var t166 string = t163 + t165
    var t167 string = t161 + t166
    retv159 = t167
    return retv159
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv169 string
    var t170 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t171 string = t170 + "#"
    var t172 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t172)
    var t174 string = t171 + t173
    retv169 = t174
    return retv169
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv176 string
    var t177 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t178 string = t177 + "#"
    var t179 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t179)
    var t181 string = t178 + t180
    retv176 = t181
    return retv176
}

func show_both__T_int32(x__21 int32) string {
    var retv183 string
    var t184 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t185 string = t184 + " / "
    var t186 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t187 string = t185 + t186
    retv183 = t187
    return retv183
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv189 string
    var t190 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t191 string = t190 + " / "
    var t192 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t193 string = t191 + t192
    retv189 = t193
    return retv189
}

func main() {
    main0()
}
