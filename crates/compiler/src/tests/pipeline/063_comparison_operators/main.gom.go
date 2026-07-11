package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func test_int_comparisons() struct{} {
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t30 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t31 string = "10 < 20: " + t30
    println__T_string(t31)
    var greater__4 bool = b__1 > a__0
    var t32 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t33 string = "20 > 10: " + t32
    println__T_string(t33)
    var less_eq1__5 bool = a__0 <= b__1
    var t34 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t35 string = "10 <= 20: " + t34
    println__T_string(t35)
    var less_eq2__6 bool = a__0 <= c__2
    var t36 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t37 string = "10 <= 10: " + t36
    println__T_string(t37)
    var greater_eq1__7 bool = b__1 >= a__0
    var t38 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t39 string = "20 >= 10: " + t38
    println__T_string(t39)
    var greater_eq2__8 bool = c__2 >= a__0
    var t40 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t41 string = "10 >= 10: " + t40
    println__T_string(t41)
    var eq1__9 bool = a__0 == c__2
    var t42 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t43 string = "10 == 10: " + t42
    println__T_string(t43)
    var eq2__10 bool = a__0 == b__1
    var t44 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t45 string = "10 == 20: " + t44
    println__T_string(t45)
    var neq1__11 bool = a__0 != b__1
    var t46 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t47 string = "10 != 20: " + t46
    println__T_string(t47)
    var neq2__12 bool = a__0 != c__2
    var t48 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t49 string = "10 != 10: " + t48
    println__T_string(t49)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t51 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t52 string = "2.71 < 3.14: " + t51
    println__T_string(t52)
    var greater__17 bool = x__13 > y__14
    var t53 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t54 string = "3.14 > 2.71: " + t53
    println__T_string(t54)
    var less_eq1__18 bool = y__14 <= x__13
    var t55 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t56 string = "2.71 <= 3.14: " + t55
    println__T_string(t56)
    var less_eq2__19 bool = x__13 <= z__15
    var t57 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t58 string = "3.14 <= 3.14: " + t57
    println__T_string(t58)
    var greater_eq1__20 bool = x__13 >= y__14
    var t59 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t60 string = "3.14 >= 2.71: " + t59
    println__T_string(t60)
    var greater_eq2__21 bool = z__15 >= x__13
    var t61 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t62 string = "3.14 >= 3.14: " + t61
    println__T_string(t62)
    var eq1__22 bool = x__13 == z__15
    var t63 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t64 string = "3.14 == 3.14: " + t63
    println__T_string(t64)
    var eq2__23 bool = x__13 == y__14
    var t65 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t66 string = "3.14 == 2.71: " + t65
    println__T_string(t66)
    var neq1__24 bool = x__13 != y__14
    var t67 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t68 string = "3.14 != 2.71: " + t67
    println__T_string(t68)
    var neq2__25 bool = x__13 != z__15
    var t69 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t70 string = "3.14 != 3.14: " + t69
    println__T_string(t70)
    return struct{}{}
}

func main0() struct{} {
    println__T_string("=== Integer Comparisons ===")
    test_int_comparisons()
    println__T_string("")
    println__T_string("=== Float Comparisons ===")
    test_float_comparisons()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv76 string
    var t77 string = _goml_runtime_core_bool_to_string(self__8)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv79 string
    retv79 = self__9
    return retv79
}

func main() {
    main0()
}
