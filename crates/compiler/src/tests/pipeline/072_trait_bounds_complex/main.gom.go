package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Boxed struct {
    value int32
}

type GoError = error

func _goml_trait_impl_Display_int32_show(self__0 int32) string {
    var retv5 string
    var t6 string = int32_to_string(self__0)
    retv5 = t6
    return retv5
}

func _goml_trait_impl_Debug_int32_show(self__1 int32) string {
    var retv8 string
    var t9 string = int32_to_string(self__1)
    var t10 string = "i32(" + t9
    var t11 string = t10 + ")"
    retv8 = t11
    return retv8
}

func _goml_trait_impl_MyEq_int32_eq(self__2 int32, other__3 int32) bool {
    var retv13 bool
    var t14 bool = self__2 == other__3
    retv13 = t14
    return retv13
}

func _goml_trait_impl_MyHash_int32_hash(self__4 int32) int32 {
    var retv16 int32
    var t17 int32 = self__4 * 16777619
    var t18 int32 = t17 + 216613626
    retv16 = t18
    return retv16
}

func _goml_trait_impl_Add_int32_add(self__5 int32, other__6 int32) int32 {
    var retv20 int32
    var t21 int32 = self__5 + other__6
    retv20 = t21
    return retv20
}

func _goml_trait_impl_Scale_int32_scale(self__7 int32, factor__8 int32) int32 {
    var retv23 int32
    var t24 int32 = self__7 * factor__8
    retv23 = t24
    return retv23
}

func _goml_trait_impl_Inspect_int32_inspect(self__9 int32) string {
    var retv26 string
    var t27 string = int32_to_string(self__9)
    var t28 string = "<" + t27
    var t29 string = t28 + ">"
    retv26 = t29
    return retv26
}

func _goml_trait_impl_Display_Boxed_show(self__10 Boxed) string {
    var retv31 string
    var t32 int32 = self__10.value
    var t33 string = int32_to_string(t32)
    var t34 string = "Boxed(" + t33
    var t35 string = t34 + ")"
    retv31 = t35
    return retv31
}

func _goml_trait_impl_Debug_Boxed_show(self__11 Boxed) string {
    var retv37 string
    var t38 int32 = self__11.value
    var t39 string = int32_to_string(t38)
    var t40 string = "Boxed{value=" + t39
    var t41 string = t40 + "}"
    retv37 = t41
    return retv37
}

func _goml_trait_impl_MyEq_Boxed_eq(self__12 Boxed, other__13 Boxed) bool {
    var retv43 bool
    var t44 int32 = self__12.value
    var t45 int32 = other__13.value
    var t46 bool = t44 == t45
    retv43 = t46
    return retv43
}

func _goml_trait_impl_MyHash_Boxed_hash(self__14 Boxed) int32 {
    var retv48 int32
    var t49 int32 = self__14.value
    var t50 int32 = t49 * 31
    var t51 int32 = t50 + 7
    var t52 int32 = t51 * 1315423911
    retv48 = t52
    return retv48
}

func _goml_trait_impl_Add_Boxed_add(self__15 Boxed, other__16 Boxed) Boxed {
    var retv54 Boxed
    var t55 int32 = self__15.value
    var t56 int32 = other__16.value
    var t57 int32 = t55 + t56
    var t58 Boxed = Boxed{
        value: t57,
    }
    retv54 = t58
    return retv54
}

func _goml_trait_impl_Scale_Boxed_scale(self__17 Boxed, factor__18 int32) Boxed {
    var retv60 Boxed
    var t61 int32 = self__17.value
    var t62 int32 = t61 * factor__18
    var t63 Boxed = Boxed{
        value: t62,
    }
    retv60 = t63
    return retv60
}

func _goml_trait_impl_Inspect_Boxed_inspect(self__19 Boxed) string {
    var retv65 string
    var t66 int32 = self__19.value
    var t67 string = int32_to_string(t66)
    var t68 string = "[" + t67
    var t69 string = t68 + "]"
    retv65 = t69
    return retv65
}

func bool_text(x__20 bool) string {
    var retv71 string
    var jp73 string
    if x__20 {
        jp73 = "true"
    } else {
        jp73 = "false"
    }
    retv71 = jp73
    return retv71
}

func main0() struct{} {
    var t75 string = full_report__Q_int32__T_int32(7, 10, 32)
    string_println(t75)
    var t76 Boxed = Boxed{
        value: 99,
    }
    var t77 Boxed = Boxed{
        value: 3,
    }
    var t78 Boxed = Boxed{
        value: 4,
    }
    var t79 string = full_report__Q_Boxed__T_Boxed(t76, t77, t78)
    string_println(t79)
    var t80 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    string_println(t80)
    var t81 Boxed = Boxed{
        value: 1,
    }
    var t82 Boxed = Boxed{
        value: 5,
    }
    var t83 Boxed = Boxed{
        value: 6,
    }
    var t84 Boxed = Boxed{
        value: 7,
    }
    var t85 string = sum_and_tag__Q_Boxed__T_Boxed(t81, t82, t83, t84)
    string_println(t85)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var retv87 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t88 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    retv87 = t88
    return retv87
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var retv90 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t91 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    retv90 = t91
    return retv90
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var retv93 string
    var t94 int32 = _goml_trait_impl_Add_int32_add(x__39, y__40)
    var total__42 int32 = _goml_trait_impl_Add_int32_add(t94, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_trait_impl_MyHash_int32_hash(total__42)
    var t95 string = header__43 + " "
    var t96 string = _goml_trait_impl_Inspect_int32_inspect(total__42)
    var t97 string = t95 + t96
    var t98 string = int32_to_string(h__44)
    var t99 string = " @" + t98
    var t100 string = t97 + t99
    retv93 = t100
    return retv93
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var retv102 string
    var t103 Boxed = _goml_trait_impl_Add_Boxed_add(x__39, y__40)
    var total__42 Boxed = _goml_trait_impl_Add_Boxed_add(t103, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_trait_impl_MyHash_Boxed_hash(total__42)
    var t104 string = header__43 + " "
    var t105 string = _goml_trait_impl_Inspect_Boxed_inspect(total__42)
    var t106 string = t104 + t105
    var t107 string = int32_to_string(h__44)
    var t108 string = " @" + t107
    var t109 string = t106 + t108
    retv102 = t109
    return retv102
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var retv111 int32
    var t112 int32 = _goml_trait_impl_Add_int32_add(a__23, b__24)
    var t113 int32 = _goml_trait_impl_Scale_int32_scale(t112, factor__25)
    retv111 = t113
    return retv111
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var retv115 string
    var same__30 bool = _goml_trait_impl_MyEq_int32_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_trait_impl_MyHash_int32_hash(combined__29)
    var t116 string = header__31 + " "
    var t117 string = t116 + repr__32
    var t118 string = bool_text(same__30)
    var t119 string = " | eq=" + t118
    var t120 string = int32_to_string(h__33)
    var t121 string = " | hash=" + t120
    var t122 string = t119 + t121
    var t123 string = t117 + t122
    retv115 = t123
    return retv115
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var retv125 Boxed
    var t126 Boxed = _goml_trait_impl_Add_Boxed_add(a__23, b__24)
    var t127 Boxed = _goml_trait_impl_Scale_Boxed_scale(t126, factor__25)
    retv125 = t127
    return retv125
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var retv129 string
    var same__30 bool = _goml_trait_impl_MyEq_Boxed_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_trait_impl_MyHash_Boxed_hash(combined__29)
    var t130 string = header__31 + " "
    var t131 string = t130 + repr__32
    var t132 string = bool_text(same__30)
    var t133 string = " | eq=" + t132
    var t134 string = int32_to_string(h__33)
    var t135 string = " | hash=" + t134
    var t136 string = t133 + t135
    var t137 string = t131 + t136
    retv129 = t137
    return retv129
}

func tag_text__Q_int32(tag__22 int32) string {
    var retv139 string
    var t140 string = _goml_trait_impl_Debug_int32_show(tag__22)
    var t141 string = t140 + "#"
    var t142 int32 = _goml_trait_impl_MyHash_int32_hash(tag__22)
    var t143 string = int32_to_string(t142)
    var t144 string = t141 + t143
    retv139 = t144
    return retv139
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var retv146 string
    var t147 string = _goml_trait_impl_Debug_Boxed_show(tag__22)
    var t148 string = t147 + "#"
    var t149 int32 = _goml_trait_impl_MyHash_Boxed_hash(tag__22)
    var t150 string = int32_to_string(t149)
    var t151 string = t148 + t150
    retv146 = t151
    return retv146
}

func show_both__T_int32(x__21 int32) string {
    var retv153 string
    var t154 string = _goml_trait_impl_Debug_int32_show(x__21)
    var t155 string = t154 + " / "
    var t156 string = _goml_trait_impl_Display_int32_show(x__21)
    var t157 string = t155 + t156
    retv153 = t157
    return retv153
}

func show_both__T_Boxed(x__21 Boxed) string {
    var retv159 string
    var t160 string = _goml_trait_impl_Debug_Boxed_show(x__21)
    var t161 string = t160 + " / "
    var t162 string = _goml_trait_impl_Display_Boxed_show(x__21)
    var t163 string = t161 + t162
    retv159 = t163
    return retv159
}

func main() {
    main0()
}
