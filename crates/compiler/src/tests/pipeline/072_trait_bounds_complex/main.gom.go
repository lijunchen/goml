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
    var ret81 string
    ret81 = int32_to_string(self__0)
    return ret81
}

func _goml_trait_impl_Debug_int32_show(self__1 int32) string {
    var ret82 string
    var t5 string = int32_to_string(self__1)
    var t4 string = "i32(" + t5
    ret82 = t4 + ")"
    return ret82
}

func _goml_trait_impl_Eq_int32_eq(self__2 int32, other__3 int32) bool {
    var ret83 bool
    ret83 = self__2 == other__3
    return ret83
}

func _goml_trait_impl_Hash_int32_hash(self__4 int32) int32 {
    var ret84 int32
    var t6 int32 = self__4 * 16777619
    ret84 = t6 + 216613626
    return ret84
}

func _goml_trait_impl_Add_int32_add(self__5 int32, other__6 int32) int32 {
    var ret85 int32
    ret85 = self__5 + other__6
    return ret85
}

func _goml_trait_impl_Scale_int32_scale(self__7 int32, factor__8 int32) int32 {
    var ret86 int32
    ret86 = self__7 * factor__8
    return ret86
}

func _goml_trait_impl_Inspect_int32_inspect(self__9 int32) string {
    var ret87 string
    var t8 string = int32_to_string(self__9)
    var t7 string = "<" + t8
    ret87 = t7 + ">"
    return ret87
}

func _goml_trait_impl_Display_Boxed_show(self__10 Boxed) string {
    var ret88 string
    var t11 int32 = self__10.value
    var t10 string = int32_to_string(t11)
    var t9 string = "Boxed(" + t10
    ret88 = t9 + ")"
    return ret88
}

func _goml_trait_impl_Debug_Boxed_show(self__11 Boxed) string {
    var ret89 string
    var t14 int32 = self__11.value
    var t13 string = int32_to_string(t14)
    var t12 string = "Boxed{value=" + t13
    ret89 = t12 + "}"
    return ret89
}

func _goml_trait_impl_Eq_Boxed_eq(self__12 Boxed, other__13 Boxed) bool {
    var ret90 bool
    var t15 int32 = self__12.value
    var t16 int32 = other__13.value
    ret90 = t15 == t16
    return ret90
}

func _goml_trait_impl_Hash_Boxed_hash(self__14 Boxed) int32 {
    var ret91 int32
    var t19 int32 = self__14.value
    var t18 int32 = t19 * 31
    var t17 int32 = t18 + 7
    ret91 = t17 * 1315423911
    return ret91
}

func _goml_trait_impl_Add_Boxed_add(self__15 Boxed, other__16 Boxed) Boxed {
    var ret92 Boxed
    var t21 int32 = self__15.value
    var t22 int32 = other__16.value
    var t20 int32 = t21 + t22
    ret92 = Boxed{
        value: t20,
    }
    return ret92
}

func _goml_trait_impl_Scale_Boxed_scale(self__17 Boxed, factor__18 int32) Boxed {
    var ret93 Boxed
    var t24 int32 = self__17.value
    var t23 int32 = t24 * factor__18
    ret93 = Boxed{
        value: t23,
    }
    return ret93
}

func _goml_trait_impl_Inspect_Boxed_inspect(self__19 Boxed) string {
    var ret94 string
    var t27 int32 = self__19.value
    var t26 string = int32_to_string(t27)
    var t25 string = "[" + t26
    ret94 = t25 + "]"
    return ret94
}

func bool_text(x__20 bool) string {
    var ret95 string
    if x__20 {
        ret95 = "true"
    } else {
        ret95 = "false"
    }
    return ret95
}

func main0() struct{} {
    var ret96 struct{}
    var t28 string = full_report__Q_int32__T_int32(7, 10, 32)
    string_println(t28)
    var t30 Boxed = Boxed{
        value: 99,
    }
    var t31 Boxed = Boxed{
        value: 3,
    }
    var t32 Boxed = Boxed{
        value: 4,
    }
    var t29 string = full_report__Q_Boxed__T_Boxed(t30, t31, t32)
    string_println(t29)
    var t33 string = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
    string_println(t33)
    var t35 Boxed = Boxed{
        value: 1,
    }
    var t36 Boxed = Boxed{
        value: 5,
    }
    var t37 Boxed = Boxed{
        value: 6,
    }
    var t38 Boxed = Boxed{
        value: 7,
    }
    var t34 string = sum_and_tag__Q_Boxed__T_Boxed(t35, t36, t37, t38)
    string_println(t34)
    ret96 = struct{}{}
    return ret96
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var ret97 string
    var combined__37 int32 = combine_scaled__T_int32(a__35, b__36, 2)
    ret97 = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
    return ret97
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var ret98 string
    var combined__37 Boxed = combine_scaled__T_Boxed(a__35, b__36, 2)
    ret98 = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
    return ret98
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var ret99 string
    var t39 int32 = _goml_trait_impl_Add_int32_add(x__39, y__40)
    var total__42 int32 = _goml_trait_impl_Add_int32_add(t39, z__41)
    var header__43 string = tag_text__Q_int32(tag__38)
    var h__44 int32 = _goml_trait_impl_Hash_int32_hash(total__42)
    var t41 string = header__43 + " "
    var t42 string = _goml_trait_impl_Inspect_int32_inspect(total__42)
    var t40 string = t41 + t42
    var t44 string = int32_to_string(h__44)
    var t43 string = " @" + t44
    ret99 = t40 + t43
    return ret99
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var ret100 string
    var t45 Boxed = _goml_trait_impl_Add_Boxed_add(x__39, y__40)
    var total__42 Boxed = _goml_trait_impl_Add_Boxed_add(t45, z__41)
    var header__43 string = tag_text__Q_Boxed(tag__38)
    var h__44 int32 = _goml_trait_impl_Hash_Boxed_hash(total__42)
    var t47 string = header__43 + " "
    var t48 string = _goml_trait_impl_Inspect_Boxed_inspect(total__42)
    var t46 string = t47 + t48
    var t50 string = int32_to_string(h__44)
    var t49 string = " @" + t50
    ret100 = t46 + t49
    return ret100
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var ret101 int32
    var t51 int32 = _goml_trait_impl_Add_int32_add(a__23, b__24)
    ret101 = _goml_trait_impl_Scale_int32_scale(t51, factor__25)
    return ret101
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var ret102 string
    var same__30 bool = _goml_trait_impl_Eq_int32_eq(a__27, b__28)
    var header__31 string = tag_text__Q_int32(tag__26)
    var repr__32 string = show_both__T_int32(combined__29)
    var h__33 int32 = _goml_trait_impl_Hash_int32_hash(combined__29)
    var t53 string = header__31 + " "
    var t52 string = t53 + repr__32
    var t56 string = bool_text(same__30)
    var t55 string = " | eq=" + t56
    var t58 string = int32_to_string(h__33)
    var t57 string = " | hash=" + t58
    var t54 string = t55 + t57
    ret102 = t52 + t54
    return ret102
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var ret103 Boxed
    var t59 Boxed = _goml_trait_impl_Add_Boxed_add(a__23, b__24)
    ret103 = _goml_trait_impl_Scale_Boxed_scale(t59, factor__25)
    return ret103
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var ret104 string
    var same__30 bool = _goml_trait_impl_Eq_Boxed_eq(a__27, b__28)
    var header__31 string = tag_text__Q_Boxed(tag__26)
    var repr__32 string = show_both__T_Boxed(combined__29)
    var h__33 int32 = _goml_trait_impl_Hash_Boxed_hash(combined__29)
    var t61 string = header__31 + " "
    var t60 string = t61 + repr__32
    var t64 string = bool_text(same__30)
    var t63 string = " | eq=" + t64
    var t66 string = int32_to_string(h__33)
    var t65 string = " | hash=" + t66
    var t62 string = t63 + t65
    ret104 = t60 + t62
    return ret104
}

func tag_text__Q_int32(tag__22 int32) string {
    var ret105 string
    var t68 string = _goml_trait_impl_Debug_int32_show(tag__22)
    var t67 string = t68 + "#"
    var t70 int32 = _goml_trait_impl_Hash_int32_hash(tag__22)
    var t69 string = int32_to_string(t70)
    ret105 = t67 + t69
    return ret105
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var ret106 string
    var t72 string = _goml_trait_impl_Debug_Boxed_show(tag__22)
    var t71 string = t72 + "#"
    var t74 int32 = _goml_trait_impl_Hash_Boxed_hash(tag__22)
    var t73 string = int32_to_string(t74)
    ret106 = t71 + t73
    return ret106
}

func show_both__T_int32(x__21 int32) string {
    var ret107 string
    var t76 string = _goml_trait_impl_Debug_int32_show(x__21)
    var t75 string = t76 + " / "
    var t77 string = _goml_trait_impl_Display_int32_show(x__21)
    ret107 = t75 + t77
    return ret107
}

func show_both__T_Boxed(x__21 Boxed) string {
    var ret108 string
    var t79 string = _goml_trait_impl_Debug_Boxed_show(x__21)
    var t78 string = t79 + " / "
    var t80 string = _goml_trait_impl_Display_Boxed_show(x__21)
    ret108 = t78 + t80
    return ret108
}

func main() {
    main0()
}
