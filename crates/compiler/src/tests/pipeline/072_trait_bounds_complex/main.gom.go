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

func goml__trait__impl_x23_Display_x23_int32_x23_show(self__0 int32) string {
    var ret65 string
    ret65 = int32_to_string(self__0)
    return ret65
}

func goml__trait__impl_x23_Debug_x23_int32_x23_show(self__1 int32) string {
    var ret66 string
    var t5 string = int32_to_string(self__1)
    var t4 string = "i32(" + t5
    ret66 = t4 + ")"
    return ret66
}

func goml__trait__impl_x23_Eq_x23_int32_x23_eq(self__2 int32, other__3 int32) bool {
    var ret67 bool
    ret67 = self__2 == other__3
    return ret67
}

func goml__trait__impl_x23_Hash_x23_int32_x23_hash(self__4 int32) int32 {
    var ret68 int32
    var t6 int32 = self__4 * 16777619
    ret68 = t6 + 216613626
    return ret68
}

func goml__trait__impl_x23_Add_x23_int32_x23_add(self__5 int32, other__6 int32) int32 {
    var ret69 int32
    ret69 = self__5 + other__6
    return ret69
}

func goml__trait__impl_x23_Scale_x23_int32_x23_scale(self__7 int32, factor__8 int32) int32 {
    var ret70 int32
    ret70 = self__7 * factor__8
    return ret70
}

func goml__trait__impl_x23_Inspect_x23_int32_x23_inspect(self__9 int32) string {
    var ret71 string
    var t8 string = int32_to_string(self__9)
    var t7 string = "<" + t8
    ret71 = t7 + ">"
    return ret71
}

func goml__trait__impl_x23_Display_x23_Boxed_x23_show(self__10 Boxed) string {
    var ret72 string
    var t11 int32 = self__10.value
    var t10 string = int32_to_string(t11)
    var t9 string = "Boxed(" + t10
    ret72 = t9 + ")"
    return ret72
}

func goml__trait__impl_x23_Debug_x23_Boxed_x23_show(self__11 Boxed) string {
    var ret73 string
    var t14 int32 = self__11.value
    var t13 string = int32_to_string(t14)
    var t12 string = "Boxed{value=" + t13
    ret73 = t12 + "}"
    return ret73
}

func goml__trait__impl_x23_Eq_x23_Boxed_x23_eq(self__12 Boxed, other__13 Boxed) bool {
    var ret74 bool
    var t15 int32 = self__12.value
    var t16 int32 = other__13.value
    ret74 = t15 == t16
    return ret74
}

func goml__trait__impl_x23_Hash_x23_Boxed_x23_hash(self__14 Boxed) int32 {
    var ret75 int32
    var t19 int32 = self__14.value
    var t18 int32 = t19 * 31
    var t17 int32 = t18 + 7
    ret75 = t17 * 1315423911
    return ret75
}

func goml__trait__impl_x23_Add_x23_Boxed_x23_add(self__15 Boxed, other__16 Boxed) Boxed {
    var ret76 Boxed
    var t21 int32 = self__15.value
    var t22 int32 = other__16.value
    var t20 int32 = t21 + t22
    ret76 = Boxed{
        value: t20,
    }
    return ret76
}

func goml__trait__impl_x23_Scale_x23_Boxed_x23_scale(self__17 Boxed, factor__18 int32) Boxed {
    var ret77 Boxed
    var t24 int32 = self__17.value
    var t23 int32 = t24 * factor__18
    ret77 = Boxed{
        value: t23,
    }
    return ret77
}

func goml__trait__impl_x23_Inspect_x23_Boxed_x23_inspect(self__19 Boxed) string {
    var ret78 string
    var t27 int32 = self__19.value
    var t26 string = int32_to_string(t27)
    var t25 string = "[" + t26
    ret78 = t25 + "]"
    return ret78
}

func bool_text(x__20 bool) string {
    var ret79 string
    if x__20 {
        ret79 = "true"
    } else {
        ret79 = "false"
    }
    return ret79
}

func main0() struct{} {
    var ret80 struct{}
    var t28 string = combine_and_render__T_int32(10, 32)
    string_println(t28)
    var t30 Boxed = Boxed{
        value: 3,
    }
    var t31 Boxed = Boxed{
        value: 4,
    }
    var t29 string = combine_and_render__T_Boxed(t30, t31)
    string_println(t29)
    var t32 string = sum_and_inspect__T_int32(1, 2, 3)
    string_println(t32)
    var t34 Boxed = Boxed{
        value: 5,
    }
    var t35 Boxed = Boxed{
        value: 6,
    }
    var t36 Boxed = Boxed{
        value: 7,
    }
    var t33 string = sum_and_inspect__T_Boxed(t34, t35, t36)
    string_println(t33)
    ret80 = struct{}{}
    return ret80
}

func combine_and_render__T_int32(a__21 int32, b__22 int32) string {
    var ret81 string
    var t37 int32 = goml__trait__impl_x23_Add_x23_int32_x23_add(a__21, b__22)
    var scaled__23 int32 = goml__trait__impl_x23_Scale_x23_int32_x23_scale(t37, 2)
    var same__24 bool = goml__trait__impl_x23_Eq_x23_int32_x23_eq(a__21, b__22)
    var t38 int32 = goml__trait__impl_x23_Hash_x23_int32_x23_hash(a__21)
    var t39 int32 = goml__trait__impl_x23_Hash_x23_int32_x23_hash(b__22)
    var sum_hash__25 int32 = t38 + t39
    var dbg__26 string = goml__trait__impl_x23_Debug_x23_int32_x23_show(scaled__23)
    var disp__27 string = goml__trait__impl_x23_Display_x23_int32_x23_show(scaled__23)
    var t42 string = dbg__26 + " | "
    var t41 string = t42 + disp__27
    var t40 string = t41 + " | eq="
    var t44 string = bool_text(same__24)
    var t46 string = int32_to_string(sum_hash__25)
    var t45 string = " | hash=" + t46
    var t43 string = t44 + t45
    ret81 = t40 + t43
    return ret81
}

func combine_and_render__T_Boxed(a__21 Boxed, b__22 Boxed) string {
    var ret82 string
    var t47 Boxed = goml__trait__impl_x23_Add_x23_Boxed_x23_add(a__21, b__22)
    var scaled__23 Boxed = goml__trait__impl_x23_Scale_x23_Boxed_x23_scale(t47, 2)
    var same__24 bool = goml__trait__impl_x23_Eq_x23_Boxed_x23_eq(a__21, b__22)
    var t48 int32 = goml__trait__impl_x23_Hash_x23_Boxed_x23_hash(a__21)
    var t49 int32 = goml__trait__impl_x23_Hash_x23_Boxed_x23_hash(b__22)
    var sum_hash__25 int32 = t48 + t49
    var dbg__26 string = goml__trait__impl_x23_Debug_x23_Boxed_x23_show(scaled__23)
    var disp__27 string = goml__trait__impl_x23_Display_x23_Boxed_x23_show(scaled__23)
    var t52 string = dbg__26 + " | "
    var t51 string = t52 + disp__27
    var t50 string = t51 + " | eq="
    var t54 string = bool_text(same__24)
    var t56 string = int32_to_string(sum_hash__25)
    var t55 string = " | hash=" + t56
    var t53 string = t54 + t55
    ret82 = t50 + t53
    return ret82
}

func sum_and_inspect__T_int32(x__28 int32, y__29 int32, z__30 int32) string {
    var ret83 string
    var t57 int32 = goml__trait__impl_x23_Add_x23_int32_x23_add(x__28, y__29)
    var total__31 int32 = goml__trait__impl_x23_Add_x23_int32_x23_add(t57, z__30)
    var h__32 int32 = goml__trait__impl_x23_Hash_x23_int32_x23_hash(total__31)
    var t59 string = goml__trait__impl_x23_Inspect_x23_int32_x23_inspect(total__31)
    var t58 string = t59 + " @"
    var t60 string = int32_to_string(h__32)
    ret83 = t58 + t60
    return ret83
}

func sum_and_inspect__T_Boxed(x__28 Boxed, y__29 Boxed, z__30 Boxed) string {
    var ret84 string
    var t61 Boxed = goml__trait__impl_x23_Add_x23_Boxed_x23_add(x__28, y__29)
    var total__31 Boxed = goml__trait__impl_x23_Add_x23_Boxed_x23_add(t61, z__30)
    var h__32 int32 = goml__trait__impl_x23_Hash_x23_Boxed_x23_hash(total__31)
    var t63 string = goml__trait__impl_x23_Inspect_x23_Boxed_x23_inspect(total__31)
    var t62 string = t63 + " @"
    var t64 string = int32_to_string(h__32)
    ret84 = t62 + t64
    return ret84
}

func main() {
    main0()
}
