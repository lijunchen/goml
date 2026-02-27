package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Boxed struct {
    value int32
}

func _goml_trait_impl_Display_int32_show(self__0 int32) string {
    var ret92 string
    ret92 = int32_to_string(self__0)
    return ret92
}

func _goml_trait_impl_Debug_int32_show(self__1 int32) string {
    var ret93 string
    var t16 string = int32_to_string(self__1)
    var t15 string = "i32(" + t16
    ret93 = t15 + ")"
    return ret93
}

func _goml_trait_impl_MyEq_int32_eq(self__2 int32, other__3 int32) bool {
    var ret94 bool
    ret94 = self__2 == other__3
    return ret94
}

func _goml_trait_impl_MyHash_int32_hash(self__4 int32) int32 {
    var ret95 int32
    var t17 int32 = self__4 * 16777619
    ret95 = t17 + 216613626
    return ret95
}

func _goml_trait_impl_Add_int32_add(self__5 int32, other__6 int32) int32 {
    var ret96 int32
    ret96 = self__5 + other__6
    return ret96
}

func _goml_trait_impl_Scale_int32_scale(self__7 int32, factor__8 int32) int32 {
    var ret97 int32
    ret97 = self__7 * factor__8
    return ret97
}

func _goml_trait_impl_Inspect_int32_inspect(self__9 int32) string {
    var ret98 string
    var t19 string = int32_to_string(self__9)
    var t18 string = "<" + t19
    ret98 = t18 + ">"
    return ret98
}

func _goml_trait_impl_Display_Boxed_show(self__10 Boxed) string {
    var ret99 string
    var t22 int32 = self__10.value
    var t21 string = int32_to_string(t22)
    var t20 string = "Boxed(" + t21
    ret99 = t20 + ")"
    return ret99
}

func _goml_trait_impl_Debug_Boxed_show(self__11 Boxed) string {
    var ret100 string
    var t25 int32 = self__11.value
    var t24 string = int32_to_string(t25)
    var t23 string = "Boxed{value=" + t24
    ret100 = t23 + "}"
    return ret100
}

func _goml_trait_impl_MyEq_Boxed_eq(self__12 Boxed, other__13 Boxed) bool {
    var ret101 bool
    var t26 int32 = self__12.value
    var t27 int32 = other__13.value
    ret101 = t26 == t27
    return ret101
}

func _goml_trait_impl_MyHash_Boxed_hash(self__14 Boxed) int32 {
    var ret102 int32
    var t30 int32 = self__14.value
    var t29 int32 = t30 * 31
    var t28 int32 = t29 + 7
    ret102 = t28 * 1315423911
    return ret102
}

func _goml_trait_impl_Add_Boxed_add(self__15 Boxed, other__16 Boxed) Boxed {
    var ret103 Boxed
    var t32 int32 = self__15.value
    var t33 int32 = other__16.value
    var t31 int32 = t32 + t33
    ret103 = Boxed{
        value: t31,
    }
    return ret103
}

func _goml_trait_impl_Scale_Boxed_scale(self__17 Boxed, factor__18 int32) Boxed {
    var ret104 Boxed
    var t35 int32 = self__17.value
    var t34 int32 = t35 * factor__18
    ret104 = Boxed{
        value: t34,
    }
    return ret104
}

func _goml_trait_impl_Inspect_Boxed_inspect(self__19 Boxed) string {
    var ret105 string
    var t38 int32 = self__19.value
    var t37 string = int32_to_string(t38)
    var t36 string = "[" + t37
    ret105 = t36 + "]"
    return ret105
}

func bool_text(x__20 bool) string {
    var ret106 string
    if x__20 {
        ret106 = "true"
    } else {
        ret106 = "false"
    }
    return ret106
}

func main0() struct{} {
    var ret107 struct{}
    var t39 string = full_report__Q_int32__T_int32(7, 10, 32)
    string_println(t39)
    var t41 Boxed = Boxed{
        value: 99,
    }
    var t42 Boxed = Boxed{
        value: 3,
    }
    var t43 Boxed = Boxed{
        value: 4,
    }
    var t40 string = full_report__Q_Boxed__T_Boxed(t41, t42, t43)
    string_println(t40)
    var t44 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    string_println(t44)
    var t46 Boxed = Boxed{
        value: 1,
    }
    var t47 Boxed = Boxed{
        value: 5,
    }
    var t48 Boxed = Boxed{
        value: 6,
    }
    var t49 Boxed = Boxed{
        value: 7,
    }
    var t45 string = sum_and_tag__Q_Boxed__T_Boxed(t46, t47, t48, t49)
    string_println(t45)
    ret107 = struct{}{}
    return ret107
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var ret108 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    ret108 = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    return ret108
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var ret109 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    ret109 = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    return ret109
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var ret110 string
    var t50 int32 = _goml_trait_impl_Add_int32_add(x__39, y__40)
    var total__42 int32 = _goml_trait_impl_Add_int32_add(t50, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_trait_impl_MyHash_int32_hash(total__42)
    var t52 string = header__43 + " "
    var t53 string = _goml_trait_impl_Inspect_int32_inspect(total__42)
    var t51 string = t52 + t53
    var t55 string = int32_to_string(h__44)
    var t54 string = " @" + t55
    ret110 = t51 + t54
    return ret110
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var ret111 string
    var t56 Boxed = _goml_trait_impl_Add_Boxed_add(x__39, y__40)
    var total__42 Boxed = _goml_trait_impl_Add_Boxed_add(t56, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_trait_impl_MyHash_Boxed_hash(total__42)
    var t58 string = header__43 + " "
    var t59 string = _goml_trait_impl_Inspect_Boxed_inspect(total__42)
    var t57 string = t58 + t59
    var t61 string = int32_to_string(h__44)
    var t60 string = " @" + t61
    ret111 = t57 + t60
    return ret111
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var ret112 int32
    var t62 int32 = _goml_trait_impl_Add_int32_add(a__23, b__24)
    ret112 = _goml_trait_impl_Scale_int32_scale(t62, factor__25)
    return ret112
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var ret113 string
    var same__30 bool = _goml_trait_impl_MyEq_int32_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_trait_impl_MyHash_int32_hash(combined__29)
    var t64 string = header__31 + " "
    var t63 string = t64 + repr__32
    var t67 string = bool_text(same__30)
    var t66 string = " | eq=" + t67
    var t69 string = int32_to_string(h__33)
    var t68 string = " | hash=" + t69
    var t65 string = t66 + t68
    ret113 = t63 + t65
    return ret113
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var ret114 Boxed
    var t70 Boxed = _goml_trait_impl_Add_Boxed_add(a__23, b__24)
    ret114 = _goml_trait_impl_Scale_Boxed_scale(t70, factor__25)
    return ret114
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var ret115 string
    var same__30 bool = _goml_trait_impl_MyEq_Boxed_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_trait_impl_MyHash_Boxed_hash(combined__29)
    var t72 string = header__31 + " "
    var t71 string = t72 + repr__32
    var t75 string = bool_text(same__30)
    var t74 string = " | eq=" + t75
    var t77 string = int32_to_string(h__33)
    var t76 string = " | hash=" + t77
    var t73 string = t74 + t76
    ret115 = t71 + t73
    return ret115
}

func tag_text__Q_int32(tag__22 int32) string {
    var ret116 string
    var t79 string = _goml_trait_impl_Debug_int32_show(tag__22)
    var t78 string = t79 + "#"
    var t81 int32 = _goml_trait_impl_MyHash_int32_hash(tag__22)
    var t80 string = int32_to_string(t81)
    ret116 = t78 + t80
    return ret116
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var ret117 string
    var t83 string = _goml_trait_impl_Debug_Boxed_show(tag__22)
    var t82 string = t83 + "#"
    var t85 int32 = _goml_trait_impl_MyHash_Boxed_hash(tag__22)
    var t84 string = int32_to_string(t85)
    ret117 = t82 + t84
    return ret117
}

func show_both__T_int32(x__21 int32) string {
    var ret118 string
    var t87 string = _goml_trait_impl_Debug_int32_show(x__21)
    var t86 string = t87 + " / "
    var t88 string = _goml_trait_impl_Display_int32_show(x__21)
    ret118 = t86 + t88
    return ret118
}

func show_both__T_Boxed(x__21 Boxed) string {
    var ret119 string
    var t90 string = _goml_trait_impl_Debug_Boxed_show(x__21)
    var t89 string = t90 + " / "
    var t91 string = _goml_trait_impl_Display_Boxed_show(x__21)
    ret119 = t89 + t91
    return ret119
}

func main() {
    main0()
}
