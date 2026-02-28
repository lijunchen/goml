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
    var t4 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = int32_to_string(self__0)
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Debug_int32_show(self__1 int32) string {
    var t5 string
    var t6 string
    var t7 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t5 = int32_to_string(self__1)
            t6 = "i32(" + t5
            t7 = t6 + ")"
            return t7
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_MyEq_int32_eq(self__2 int32, other__3 int32) bool {
    var t8 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t8 = self__2 == other__3
            return t8
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_MyHash_int32_hash(self__4 int32) int32 {
    var t9 int32
    var t10 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t9 = self__4 * 16777619
            t10 = t9 + 216613626
            return t10
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Add_int32_add(self__5 int32, other__6 int32) int32 {
    var t11 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = self__5 + other__6
            return t11
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Scale_int32_scale(self__7 int32, factor__8 int32) int32 {
    var t12 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t12 = self__7 * factor__8
            return t12
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Inspect_int32_inspect(self__9 int32) string {
    var t13 string
    var t14 string
    var t15 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t13 = int32_to_string(self__9)
            t14 = "<" + t13
            t15 = t14 + ">"
            return t15
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Boxed_show(self__10 Boxed) string {
    var t16 int32
    var t17 string
    var t18 string
    var t19 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t16 = self__10.value
            t17 = int32_to_string(t16)
            t18 = "Boxed(" + t17
            t19 = t18 + ")"
            return t19
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Debug_Boxed_show(self__11 Boxed) string {
    var t20 int32
    var t21 string
    var t22 string
    var t23 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t20 = self__11.value
            t21 = int32_to_string(t20)
            t22 = "Boxed{value=" + t21
            t23 = t22 + "}"
            return t23
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_MyEq_Boxed_eq(self__12 Boxed, other__13 Boxed) bool {
    var t24 int32
    var t25 int32
    var t26 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t24 = self__12.value
            t25 = other__13.value
            t26 = t24 == t25
            return t26
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_MyHash_Boxed_hash(self__14 Boxed) int32 {
    var t27 int32
    var t28 int32
    var t29 int32
    var t30 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t27 = self__14.value
            t28 = t27 * 31
            t29 = t28 + 7
            t30 = t29 * 1315423911
            return t30
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Add_Boxed_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t31 int32
    var t32 int32
    var t33 int32
    var t34 Boxed
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t31 = self__15.value
            t32 = other__16.value
            t33 = t31 + t32
            t34 = Boxed{
                value: t33,
            }
            return t34
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Scale_Boxed_scale(self__17 Boxed, factor__18 int32) Boxed {
    var t35 int32
    var t36 int32
    var t37 Boxed
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t35 = self__17.value
            t36 = t35 * factor__18
            t37 = Boxed{
                value: t36,
            }
            return t37
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Inspect_Boxed_inspect(self__19 Boxed) string {
    var t38 int32
    var t39 string
    var t40 string
    var t41 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t38 = self__19.value
            t39 = int32_to_string(t38)
            t40 = "[" + t39
            t41 = t40 + "]"
            return t41
        default:
            panic("invalid pc")
        }
    }
}

func bool_text(x__20 bool) string {
    var jp43 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            if x__20 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp43
        case 2:
            jp43 = "true"
            pc = 1
        case 3:
            jp43 = "false"
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t44 string
    var mtmp0 struct{}
    var t45 Boxed
    var t46 Boxed
    var t47 Boxed
    var t48 string
    var mtmp1 struct{}
    var t49 string
    var mtmp2 struct{}
    var t50 Boxed
    var t51 Boxed
    var t52 Boxed
    var t53 Boxed
    var t54 string
    var mtmp3 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t44 = full_report__Q_int32__T_int32(7, 10, 32)
            string_println(t44)
            t45 = Boxed{
                value: 99,
            }
            t46 = Boxed{
                value: 3,
            }
            t47 = Boxed{
                value: 4,
            }
            t48 = full_report__Q_Boxed__T_Boxed(t45, t46, t47)
            string_println(t48)
            t49 = sum_and_tag__Q_int32__T_int32(0, 1, 2, 3)
            string_println(t49)
            t50 = Boxed{
                value: 1,
            }
            t51 = Boxed{
                value: 5,
            }
            t52 = Boxed{
                value: 6,
            }
            t53 = Boxed{
                value: 7,
            }
            t54 = sum_and_tag__Q_Boxed__T_Boxed(t50, t51, t52, t53)
            string_println(t54)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func full_report__Q_int32__T_int32(tag__34 int32, a__35 int32, b__36 int32) string {
    var combined__37 int32
    var t55 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            combined__37 = combine_scaled__T_int32(a__35, b__36, 2)
            t55 = report_pair__Q_int32__T_int32(tag__34, a__35, b__36, combined__37)
            return t55
        default:
            panic("invalid pc")
        }
    }
}

func full_report__Q_Boxed__T_Boxed(tag__34 Boxed, a__35 Boxed, b__36 Boxed) string {
    var combined__37 Boxed
    var t56 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            combined__37 = combine_scaled__T_Boxed(a__35, b__36, 2)
            t56 = report_pair__Q_Boxed__T_Boxed(tag__34, a__35, b__36, combined__37)
            return t56
        default:
            panic("invalid pc")
        }
    }
}

func sum_and_tag__Q_int32__T_int32(tag__38 int32, x__39 int32, y__40 int32, z__41 int32) string {
    var t57 int32
    var total__42 int32
    var header__43 string
    var h__44 int32
    var t58 string
    var t59 string
    var t60 string
    var t61 string
    var t62 string
    var t63 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t57 = _goml_trait_impl_Add_int32_add(x__39, y__40)
            total__42 = _goml_trait_impl_Add_int32_add(t57, z__41)
            header__43 = tag_text__Q_int32(tag__38)
            h__44 = _goml_trait_impl_MyHash_int32_hash(total__42)
            t58 = header__43 + " "
            t59 = _goml_trait_impl_Inspect_int32_inspect(total__42)
            t60 = t58 + t59
            t61 = int32_to_string(h__44)
            t62 = " @" + t61
            t63 = t60 + t62
            return t63
        default:
            panic("invalid pc")
        }
    }
}

func sum_and_tag__Q_Boxed__T_Boxed(tag__38 Boxed, x__39 Boxed, y__40 Boxed, z__41 Boxed) string {
    var t64 Boxed
    var total__42 Boxed
    var header__43 string
    var h__44 int32
    var t65 string
    var t66 string
    var t67 string
    var t68 string
    var t69 string
    var t70 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t64 = _goml_trait_impl_Add_Boxed_add(x__39, y__40)
            total__42 = _goml_trait_impl_Add_Boxed_add(t64, z__41)
            header__43 = tag_text__Q_Boxed(tag__38)
            h__44 = _goml_trait_impl_MyHash_Boxed_hash(total__42)
            t65 = header__43 + " "
            t66 = _goml_trait_impl_Inspect_Boxed_inspect(total__42)
            t67 = t65 + t66
            t68 = int32_to_string(h__44)
            t69 = " @" + t68
            t70 = t67 + t69
            return t70
        default:
            panic("invalid pc")
        }
    }
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t71 int32
    var t72 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t71 = _goml_trait_impl_Add_int32_add(a__23, b__24)
            t72 = _goml_trait_impl_Scale_int32_scale(t71, factor__25)
            return t72
        default:
            panic("invalid pc")
        }
    }
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var header__31 string
    var repr__32 string
    var h__33 int32
    var t73 string
    var t74 string
    var t75 string
    var t76 string
    var t77 string
    var t78 string
    var t79 string
    var t80 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            same__30 = _goml_trait_impl_MyEq_int32_eq(a__27, b__28)
            header__31 = tag_text__Q_int32(tag__26)
            repr__32 = show_both__T_int32(combined__29)
            h__33 = _goml_trait_impl_MyHash_int32_hash(combined__29)
            t73 = header__31 + " "
            t74 = t73 + repr__32
            t75 = bool_text(same__30)
            t76 = " | eq=" + t75
            t77 = int32_to_string(h__33)
            t78 = " | hash=" + t77
            t79 = t76 + t78
            t80 = t74 + t79
            return t80
        default:
            panic("invalid pc")
        }
    }
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t81 Boxed
    var t82 Boxed
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t81 = _goml_trait_impl_Add_Boxed_add(a__23, b__24)
            t82 = _goml_trait_impl_Scale_Boxed_scale(t81, factor__25)
            return t82
        default:
            panic("invalid pc")
        }
    }
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var header__31 string
    var repr__32 string
    var h__33 int32
    var t83 string
    var t84 string
    var t85 string
    var t86 string
    var t87 string
    var t88 string
    var t89 string
    var t90 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            same__30 = _goml_trait_impl_MyEq_Boxed_eq(a__27, b__28)
            header__31 = tag_text__Q_Boxed(tag__26)
            repr__32 = show_both__T_Boxed(combined__29)
            h__33 = _goml_trait_impl_MyHash_Boxed_hash(combined__29)
            t83 = header__31 + " "
            t84 = t83 + repr__32
            t85 = bool_text(same__30)
            t86 = " | eq=" + t85
            t87 = int32_to_string(h__33)
            t88 = " | hash=" + t87
            t89 = t86 + t88
            t90 = t84 + t89
            return t90
        default:
            panic("invalid pc")
        }
    }
}

func tag_text__Q_int32(tag__22 int32) string {
    var t91 string
    var t92 string
    var t93 int32
    var t94 string
    var t95 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t91 = _goml_trait_impl_Debug_int32_show(tag__22)
            t92 = t91 + "#"
            t93 = _goml_trait_impl_MyHash_int32_hash(tag__22)
            t94 = int32_to_string(t93)
            t95 = t92 + t94
            return t95
        default:
            panic("invalid pc")
        }
    }
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t96 string
    var t97 string
    var t98 int32
    var t99 string
    var t100 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t96 = _goml_trait_impl_Debug_Boxed_show(tag__22)
            t97 = t96 + "#"
            t98 = _goml_trait_impl_MyHash_Boxed_hash(tag__22)
            t99 = int32_to_string(t98)
            t100 = t97 + t99
            return t100
        default:
            panic("invalid pc")
        }
    }
}

func show_both__T_int32(x__21 int32) string {
    var t101 string
    var t102 string
    var t103 string
    var t104 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t101 = _goml_trait_impl_Debug_int32_show(x__21)
            t102 = t101 + " / "
            t103 = _goml_trait_impl_Display_int32_show(x__21)
            t104 = t102 + t103
            return t104
        default:
            panic("invalid pc")
        }
    }
}

func show_both__T_Boxed(x__21 Boxed) string {
    var t105 string
    var t106 string
    var t107 string
    var t108 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t105 = _goml_trait_impl_Debug_Boxed_show(x__21)
            t106 = t105 + " / "
            t107 = _goml_trait_impl_Display_Boxed_show(x__21)
            t108 = t106 + t107
            return t108
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
