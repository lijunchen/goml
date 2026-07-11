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
    var t33 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t34 string = "10 < 20: " + t33
    println__T_string(t34)
    var greater__4 bool = b__1 > a__0
    var t35 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t36 string = "20 > 10: " + t35
    println__T_string(t36)
    var less_eq1__5 bool = a__0 <= b__1
    var t37 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t38 string = "10 <= 20: " + t37
    println__T_string(t38)
    var less_eq2__6 bool = a__0 <= c__2
    var t39 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t40 string = "10 <= 10: " + t39
    println__T_string(t40)
    var greater_eq1__7 bool = b__1 >= a__0
    var t41 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t42 string = "20 >= 10: " + t41
    println__T_string(t42)
    var greater_eq2__8 bool = c__2 >= a__0
    var t43 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t44 string = "10 >= 10: " + t43
    println__T_string(t44)
    var eq1__9 bool = a__0 == c__2
    var t45 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t46 string = "10 == 10: " + t45
    println__T_string(t46)
    var eq2__10 bool = a__0 == b__1
    var t47 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t48 string = "10 == 20: " + t47
    println__T_string(t48)
    var neq1__11 bool = a__0 != b__1
    var t49 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t50 string = "10 != 20: " + t49
    println__T_string(t50)
    var neq2__12 bool = a__0 != c__2
    var t51 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t52 string = "10 != 10: " + t51
    println__T_string(t52)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t54 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t55 string = "2.71 < 3.14: " + t54
    println__T_string(t55)
    var greater__17 bool = x__13 > y__14
    var t56 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t57 string = "3.14 > 2.71: " + t56
    println__T_string(t57)
    var less_eq1__18 bool = y__14 <= x__13
    var t58 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t59 string = "2.71 <= 3.14: " + t58
    println__T_string(t59)
    var less_eq2__19 bool = x__13 <= z__15
    var t60 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t61 string = "3.14 <= 3.14: " + t60
    println__T_string(t61)
    var greater_eq1__20 bool = x__13 >= y__14
    var t62 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t63 string = "3.14 >= 2.71: " + t62
    println__T_string(t63)
    var greater_eq2__21 bool = z__15 >= x__13
    var t64 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t65 string = "3.14 >= 3.14: " + t64
    println__T_string(t65)
    var eq1__22 bool = x__13 == z__15
    var t66 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t67 string = "3.14 == 3.14: " + t66
    println__T_string(t67)
    var eq2__23 bool = x__13 == y__14
    var t68 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t69 string = "3.14 == 2.71: " + t68
    println__T_string(t69)
    var neq1__24 bool = x__13 != y__14
    var t70 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t71 string = "3.14 != 2.71: " + t70
    println__T_string(t71)
    var neq2__25 bool = x__13 != z__15
    var t72 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t73 string = "3.14 != 3.14: " + t72
    println__T_string(t73)
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
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv79 string
    var t80 string = _goml_runtime_core_bool_to_string(self__8)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv82 string
    retv82 = self__9
    return retv82
}

func main() {
    main0()
}
