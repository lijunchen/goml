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
    var retv9 string
    var t10 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv9 = t10
    return retv9
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var retv12 string
    var t13 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__1)
    var t14 string = "i32(" + t13
    var t15 string = t14 + ")"
    retv12 = t15
    return retv12
}

func _goml_m_trait__impl_i_MyEq_i_int32_i_eq(self__2 int32, other__3 int32) bool {
    var retv17 bool
    var t18 bool = self__2 == other__3
    retv17 = t18
    return retv17
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var retv20 int32
    var t21 int32 = self__4 * 16777619
    var t22 int32 = t21 + 216613626
    retv20 = t22
    return retv20
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var retv24 int32
    var t25 int32 = self__5 + other__6
    retv24 = t25
    return retv24
}

func _goml_m_trait__impl_i_Scale_i_int32_i_scale(self__7 int32, factor__8 int32) int32 {
    var retv27 int32
    var t28 int32 = self__7 * factor__8
    retv27 = t28
    return retv27
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var retv30 string
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__9)
    var t32 string = "<" + t31
    var t33 string = t32 + ">"
    retv30 = t33
    return retv30
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var retv35 string
    var t36 int32 = self__10.value
    var t37 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t36)
    var t38 string = "Boxed(" + t37
    var t39 string = t38 + ")"
    retv35 = t39
    return retv35
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var retv41 string
    var t42 int32 = self__11.value
    var t43 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t42)
    var t44 string = "Boxed{value=" + t43
    var t45 string = t44 + "}"
    retv41 = t45
    return retv41
}

func _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv47 bool
    var t48 int32 = self__12.value
    var t49 int32 = other__13.value
    var t50 bool = t48 == t49
    retv47 = t50
    return retv47
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var retv52 int32
    var t53 int32 = self__14.value
    var t54 int32 = t53 * 31
    var t55 int32 = t54 + 7
    var t56 int32 = t55 * 1315423911
    retv52 = t56
    return retv52
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv58 Boxed
    var t59 int32 = self__15.value
    var t60 int32 = other__16.value
    var t61 int32 = t59 + t60
    var t62 Boxed = Boxed{
        value: t61,
    }
    retv58 = t62
    return retv58
}

func _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv64 Boxed
    var t65 int32 = self__17.value
    var t66 int32 = t65 * factor__18
    var t67 Boxed = Boxed{
        value: t66,
    }
    retv64 = t67
    return retv64
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var retv69 string
    var t70 int32 = self__19.value
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t70)
    var t72 string = "[" + t71
    var t73 string = t72 + "]"
    retv69 = t73
    return retv69
}

func bool_text(x__20 bool) string {
    var retv75 string
    var jp77 string
    if x__20 {
        jp77 = "true"
    } else {
        jp77 = "false"
    }
    retv75 = jp77
    return retv75
}

func main0() struct{} {
    var t79 string = full_report__Q_int32__T_int32(7, 10, 32)
    println__T_string(t79)
    var t80 Boxed = Boxed{
        value: 99,
    }
    var t81 Boxed = Boxed{
        value: 3,
    }
    var t82 Boxed = Boxed{
        value: 4,
    }
    var t83 string = full_report__Q_Boxed__T_Boxed(t80, t81, t82)
    println__T_string(t83)
    var t84 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    println__T_string(t84)
    var t85 Boxed = Boxed{
        value: 1,
    }
    var t86 Boxed = Boxed{
        value: 5,
    }
    var t87 Boxed = Boxed{
        value: 6,
    }
    var t88 Boxed = Boxed{
        value: 7,
    }
    var t89 string = sum_and_tag__Q_Boxed__T_Boxed(t85, t86, t87, t88)
    println__T_string(t89)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__2)
    retv91 = t92
    return retv91
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv97 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t98 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv97 = t98
    return retv97
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv100 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t101 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv100 = t101
    return retv100
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv103 string
    var t104 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(x__39, y__40)
    var total__42 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(t104, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(total__42)
    var t105 string = header__43 + " "
    var t106 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(total__42)
    var t107 string = t105 + t106
    var t108 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t109 string = " @" + t108
    var t110 string = t107 + t109
    retv103 = t110
    return retv103
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv112 string
    var t113 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(x__39, y__40)
    var total__42 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t113, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(total__42)
    var t114 string = header__43 + " "
    var t115 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(total__42)
    var t116 string = t114 + t115
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__44)
    var t118 string = " @" + t117
    var t119 string = t116 + t118
    retv112 = t119
    return retv112
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv121 string
    retv121 = self__9
    return retv121
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv123 int32
    var t124 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(a__23, b__24)
    var t125 int32 = _goml_m_trait__impl_i_Scale_i_int32_i_scale(t124, factor__25)
    retv123 = t125
    return retv123
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv127 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_int32_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(combined__29)
    var t128 string = header__31 + " "
    var t129 string = t128 + repr__32
    var t130 string = bool_text(same__30)
    var t131 string = " | eq=" + t130
    var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t133 string = " | hash=" + t132
    var t134 string = t131 + t133
    var t135 string = t129 + t134
    retv127 = t135
    return retv127
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv137 Boxed
    var t138 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(a__23, b__24)
    var t139 Boxed = _goml_m_trait__impl_i_Scale_i_Boxed_i_scale(t138, factor__25)
    retv137 = t139
    return retv137
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv141 string
    var same__30 bool = _goml_m_trait__impl_i_MyEq_i_Boxed_i_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(combined__29)
    var t142 string = header__31 + " "
    var t143 string = t142 + repr__32
    var t144 string = bool_text(same__30)
    var t145 string = " | eq=" + t144
    var t146 string = _goml_m_inherent_i_int32_i_int32_i_to__string(h__33)
    var t147 string = " | hash=" + t146
    var t148 string = t145 + t147
    var t149 string = t143 + t148
    retv141 = t149
    return retv141
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv151 string
    var t152 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__22)
    var t153 string = t152 + "#"
    var t154 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__22)
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t154)
    var t156 string = t153 + t155
    retv151 = t156
    return retv151
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv158 string
    var t159 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__22)
    var t160 string = t159 + "#"
    var t161 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__22)
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t161)
    var t163 string = t160 + t162
    retv158 = t163
    return retv158
}

func show_both__T_int32(x__21 int32) string {
    var retv165 string
    var t166 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(x__21)
    var t167 string = t166 + " / "
    var t168 string = _goml_m_trait__impl_i_Display_i_int32_i_show(x__21)
    var t169 string = t167 + t168
    retv165 = t169
    return retv165
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv171 string
    var t172 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(x__21)
    var t173 string = t172 + " / "
    var t174 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(x__21)
    var t175 string = t173 + t174
    retv171 = t175
    return retv171
}

func main() {
    main0()
}
