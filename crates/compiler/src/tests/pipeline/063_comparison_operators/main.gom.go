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
    var t48 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t49 string = "10 < 20: " + t48
    println__T_string(t49)
    var greater__4 bool = b__1 > a__0
    var t50 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__4)
    var t51 string = "20 > 10: " + t50
    println__T_string(t51)
    var less_eq1__5 bool = a__0 <= b__1
    var t52 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__5)
    var t53 string = "10 <= 20: " + t52
    println__T_string(t53)
    var less_eq2__6 bool = a__0 <= c__2
    var t54 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__6)
    var t55 string = "10 <= 10: " + t54
    println__T_string(t55)
    var greater_eq1__7 bool = b__1 >= a__0
    var t56 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__7)
    var t57 string = "20 >= 10: " + t56
    println__T_string(t57)
    var greater_eq2__8 bool = c__2 >= a__0
    var t58 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__8)
    var t59 string = "10 >= 10: " + t58
    println__T_string(t59)
    var eq1__9 bool = a__0 == c__2
    var t60 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__9)
    var t61 string = "10 == 10: " + t60
    println__T_string(t61)
    var eq2__10 bool = a__0 == b__1
    var t62 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__10)
    var t63 string = "10 == 20: " + t62
    println__T_string(t63)
    var neq1__11 bool = a__0 != b__1
    var t64 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__11)
    var t65 string = "10 != 20: " + t64
    println__T_string(t65)
    var neq2__12 bool = a__0 != c__2
    var t66 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__12)
    var t67 string = "10 != 10: " + t66
    println__T_string(t67)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t69 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t70 string = "2.71 < 3.14: " + t69
    println__T_string(t70)
    var greater__17 bool = x__13 > y__14
    var t71 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater__17)
    var t72 string = "3.14 > 2.71: " + t71
    println__T_string(t72)
    var less_eq1__18 bool = y__14 <= x__13
    var t73 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq1__18)
    var t74 string = "2.71 <= 3.14: " + t73
    println__T_string(t74)
    var less_eq2__19 bool = x__13 <= z__15
    var t75 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_eq2__19)
    var t76 string = "3.14 <= 3.14: " + t75
    println__T_string(t76)
    var greater_eq1__20 bool = x__13 >= y__14
    var t77 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq1__20)
    var t78 string = "3.14 >= 2.71: " + t77
    println__T_string(t78)
    var greater_eq2__21 bool = z__15 >= x__13
    var t79 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(greater_eq2__21)
    var t80 string = "3.14 >= 3.14: " + t79
    println__T_string(t80)
    var eq1__22 bool = x__13 == z__15
    var t81 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq1__22)
    var t82 string = "3.14 == 3.14: " + t81
    println__T_string(t82)
    var eq2__23 bool = x__13 == y__14
    var t83 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(eq2__23)
    var t84 string = "3.14 == 2.71: " + t83
    println__T_string(t84)
    var neq1__24 bool = x__13 != y__14
    var t85 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq1__24)
    var t86 string = "3.14 != 2.71: " + t85
    println__T_string(t86)
    var neq2__25 bool = x__13 != z__15
    var t87 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(neq2__25)
    var t88 string = "3.14 != 3.14: " + t87
    println__T_string(t88)
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
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv94 string
    var t95 string = _goml_runtime_core_bool_to_string(self__8)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv97 string
    retv97 = self__9
    return retv97
}

func main() {
    main0()
}
