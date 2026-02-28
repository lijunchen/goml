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
    var t4 string = int32_to_string(self__0)
    return t4
}

func _goml_trait_impl_Debug_int32_show(self__1 int32) string {
    var t5 string = int32_to_string(self__1)
    var t6 string = "i32(" + t5
    var t7 string = t6 + ")"
    return t7
}

func _goml_trait_impl_MyEq_int32_eq(self__2 int32, other__3 int32) bool {
    var t8 bool = self__2 == other__3
    return t8
}

func _goml_trait_impl_MyHash_int32_hash(self__4 int32) int32 {
    var t9 int32 = self__4 * 16777619
    var t10 int32 = t9 + 216613626
    return t10
}

func _goml_trait_impl_Add_int32_add(self__5 int32, other__6 int32) int32 {
    var t11 int32 = self__5 + other__6
    return t11
}

func _goml_trait_impl_Scale_int32_scale(self__7 int32, factor__8 int32) int32 {
    var t12 int32 = self__7 * factor__8
    return t12
}

func _goml_trait_impl_Inspect_int32_inspect(self__9 int32) string {
    var t13 string = int32_to_string(self__9)
    var t14 string = "<" + t13
    var t15 string = t14 + ">"
    return t15
}

func _goml_trait_impl_Display_Boxed_show(self__10 Boxed) string {
    var t16 int32 = self__10.value
    var t17 string = int32_to_string(t16)
    var t18 string = "Boxed(" + t17
    var t19 string = t18 + ")"
    return t19
}

func _goml_trait_impl_Debug_Boxed_show(self__11 Boxed) string {
    var t20 int32 = self__11.value
    var t21 string = int32_to_string(t20)
    var t22 string = "Boxed{value=" + t21
    var t23 string = t22 + "}"
    return t23
}

func _goml_trait_impl_MyEq_Boxed_eq(self__12 Boxed, other__13 Boxed) bool {
    var t24 int32 = self__12.value
    var t25 int32 = other__13.value
    var t26 bool = t24 == t25
    return t26
}

func _goml_trait_impl_MyHash_Boxed_hash(self__14 Boxed) int32 {
    var t27 int32 = self__14.value
    var t28 int32 = t27 * 31
    var t29 int32 = t28 + 7
    var t30 int32 = t29 * 1315423911
    return t30
}

func _goml_trait_impl_Add_Boxed_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t31 int32 = self__15.value
    var t32 int32 = other__16.value
    var t33 int32 = t31 + t32
    var t34 Boxed = Boxed{
        value: t33,
    }
    return t34
}

func _goml_trait_impl_Scale_Boxed_scale(self__17 Boxed, factor__18 int32) Boxed {
    var t35 int32 = self__17.value
    var t36 int32 = t35 * factor__18
    var t37 Boxed = Boxed{
        value: t36,
    }
    return t37
}

func _goml_trait_impl_Inspect_Boxed_inspect(self__19 Boxed) string {
    var t38 int32 = self__19.value
    var t39 string = int32_to_string(t38)
    var t40 string = "[" + t39
    var t41 string = t40 + "]"
    return t41
}

func bool_text(x__20 bool) string {
    var jp43 string
    if x__20 {
        jp43 = "true"
    } else {
        jp43 = "false"
    }
    return jp43
}

func main0() struct{} {
    var t44 string = full_report__Q_int32__T_int32(7, 10, 32)
    string_println(t44)
    var t45 Boxed = Boxed{
        value: 99,
    }
    var t46 Boxed = Boxed{
        value: 3,
    }
    var t47 Boxed = Boxed{
        value: 4,
    }
    var t48 string = full_report__Q_Boxed__T_Boxed(t45, t46, t47)
    string_println(t48)
    var t49 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    string_println(t49)
    var t50 Boxed = Boxed{
        value: 1,
    }
    var t51 Boxed = Boxed{
        value: 5,
    }
    var t52 Boxed = Boxed{
        value: 6,
    }
    var t53 Boxed = Boxed{
        value: 7,
    }
    var t54 string = sum_and_tag__Q_Boxed__T_Boxed(t50, t51, t52, t53)
    string_println(t54)
    return struct{}{}
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    var t55 string = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    return t55
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    var t56 string = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    return t56
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var t57 int32 = _goml_trait_impl_Add_int32_add(x__39, y__40)
    var total__42 int32 = _goml_trait_impl_Add_int32_add(t57, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_trait_impl_MyHash_int32_hash(total__42)
    var t58 string = header__43 + " "
    var t59 string = _goml_trait_impl_Inspect_int32_inspect(total__42)
    var t60 string = t58 + t59
    var t61 string = int32_to_string(h__44)
    var t62 string = " @" + t61
    var t63 string = t60 + t62
    return t63
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var t64 Boxed = _goml_trait_impl_Add_Boxed_add(x__39, y__40)
    var total__42 Boxed = _goml_trait_impl_Add_Boxed_add(t64, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_trait_impl_MyHash_Boxed_hash(total__42)
    var t65 string = header__43 + " "
    var t66 string = _goml_trait_impl_Inspect_Boxed_inspect(total__42)
    var t67 string = t65 + t66
    var t68 string = int32_to_string(h__44)
    var t69 string = " @" + t68
    var t70 string = t67 + t69
    return t70
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t71 int32 = _goml_trait_impl_Add_int32_add(a__23, b__24)
    var t72 int32 = _goml_trait_impl_Scale_int32_scale(t71, factor__25)
    return t72
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool = _goml_trait_impl_MyEq_int32_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_trait_impl_MyHash_int32_hash(combined__29)
    var t73 string = header__31 + " "
    var t74 string = t73 + repr__32
    var t75 string = bool_text(same__30)
    var t76 string = " | eq=" + t75
    var t77 string = int32_to_string(h__33)
    var t78 string = " | hash=" + t77
    var t79 string = t76 + t78
    var t80 string = t74 + t79
    return t80
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t81 Boxed = _goml_trait_impl_Add_Boxed_add(a__23, b__24)
    var t82 Boxed = _goml_trait_impl_Scale_Boxed_scale(t81, factor__25)
    return t82
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool = _goml_trait_impl_MyEq_Boxed_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_trait_impl_MyHash_Boxed_hash(combined__29)
    var t83 string = header__31 + " "
    var t84 string = t83 + repr__32
    var t85 string = bool_text(same__30)
    var t86 string = " | eq=" + t85
    var t87 string = int32_to_string(h__33)
    var t88 string = " | hash=" + t87
    var t89 string = t86 + t88
    var t90 string = t84 + t89
    return t90
}

func tag_text__Q_int32(tag__22 int32) string {
    var t91 string = _goml_trait_impl_Debug_int32_show(tag__22)
    var t92 string = t91 + "#"
    var t93 int32 = _goml_trait_impl_MyHash_int32_hash(tag__22)
    var t94 string = int32_to_string(t93)
    var t95 string = t92 + t94
    return t95
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t96 string = _goml_trait_impl_Debug_Boxed_show(tag__22)
    var t97 string = t96 + "#"
    var t98 int32 = _goml_trait_impl_MyHash_Boxed_hash(tag__22)
    var t99 string = int32_to_string(t98)
    var t100 string = t97 + t99
    return t100
}

func show_both__T_int32(x__21 int32) string {
    var t101 string = _goml_trait_impl_Debug_int32_show(x__21)
    var t102 string = t101 + " / "
    var t103 string = _goml_trait_impl_Display_int32_show(x__21)
    var t104 string = t102 + t103
    return t104
}

func show_both__T_Boxed(x__21 Boxed) string {
    var t105 string = _goml_trait_impl_Debug_Boxed_show(x__21)
    var t106 string = t105 + " / "
    var t107 string = _goml_trait_impl_Display_Boxed_show(x__21)
    var t108 string = t106 + t107
    return t108
}

func main() {
    main0()
}
