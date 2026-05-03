package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func test_int_comparisons() struct{} {
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t26 string = bool_to_string(less__3)
    var t27 string = "10 < 20: " + t26
    println__T_string(t27)
    var greater__4 bool = b__1 > a__0
    var t28 string = bool_to_string(greater__4)
    var t29 string = "20 > 10: " + t28
    println__T_string(t29)
    var less_eq1__5 bool = a__0 <= b__1
    var t30 string = bool_to_string(less_eq1__5)
    var t31 string = "10 <= 20: " + t30
    println__T_string(t31)
    var less_eq2__6 bool = a__0 <= c__2
    var t32 string = bool_to_string(less_eq2__6)
    var t33 string = "10 <= 10: " + t32
    println__T_string(t33)
    var greater_eq1__7 bool = b__1 >= a__0
    var t34 string = bool_to_string(greater_eq1__7)
    var t35 string = "20 >= 10: " + t34
    println__T_string(t35)
    var greater_eq2__8 bool = c__2 >= a__0
    var t36 string = bool_to_string(greater_eq2__8)
    var t37 string = "10 >= 10: " + t36
    println__T_string(t37)
    var eq1__9 bool = a__0 == c__2
    var t38 string = bool_to_string(eq1__9)
    var t39 string = "10 == 10: " + t38
    println__T_string(t39)
    var eq2__10 bool = a__0 == b__1
    var t40 string = bool_to_string(eq2__10)
    var t41 string = "10 == 20: " + t40
    println__T_string(t41)
    var neq1__11 bool = a__0 != b__1
    var t42 string = bool_to_string(neq1__11)
    var t43 string = "10 != 20: " + t42
    println__T_string(t43)
    var neq2__12 bool = a__0 != c__2
    var t44 string = bool_to_string(neq2__12)
    var t45 string = "10 != 10: " + t44
    println__T_string(t45)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t47 string = bool_to_string(less__16)
    var t48 string = "2.71 < 3.14: " + t47
    println__T_string(t48)
    var greater__17 bool = x__13 > y__14
    var t49 string = bool_to_string(greater__17)
    var t50 string = "3.14 > 2.71: " + t49
    println__T_string(t50)
    var less_eq1__18 bool = y__14 <= x__13
    var t51 string = bool_to_string(less_eq1__18)
    var t52 string = "2.71 <= 3.14: " + t51
    println__T_string(t52)
    var less_eq2__19 bool = x__13 <= z__15
    var t53 string = bool_to_string(less_eq2__19)
    var t54 string = "3.14 <= 3.14: " + t53
    println__T_string(t54)
    var greater_eq1__20 bool = x__13 >= y__14
    var t55 string = bool_to_string(greater_eq1__20)
    var t56 string = "3.14 >= 2.71: " + t55
    println__T_string(t56)
    var greater_eq2__21 bool = z__15 >= x__13
    var t57 string = bool_to_string(greater_eq2__21)
    var t58 string = "3.14 >= 3.14: " + t57
    println__T_string(t58)
    var eq1__22 bool = x__13 == z__15
    var t59 string = bool_to_string(eq1__22)
    var t60 string = "3.14 == 3.14: " + t59
    println__T_string(t60)
    var eq2__23 bool = x__13 == y__14
    var t61 string = bool_to_string(eq2__23)
    var t62 string = "3.14 == 2.71: " + t61
    println__T_string(t62)
    var neq1__24 bool = x__13 != y__14
    var t63 string = bool_to_string(neq1__24)
    var t64 string = "3.14 != 2.71: " + t63
    println__T_string(t64)
    var neq2__25 bool = x__13 != z__15
    var t65 string = bool_to_string(neq2__25)
    var t66 string = "3.14 != 3.14: " + t65
    println__T_string(t66)
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
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
