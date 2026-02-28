package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func test_int_comparisons() struct{} {
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t25 string = bool_to_string(less__3)
    var t26 string = "10 < 20: " + t25
    string_println(t26)
    var greater__4 bool = b__1 > a__0
    var t27 string = bool_to_string(greater__4)
    var t28 string = "20 > 10: " + t27
    string_println(t28)
    var less_eq1__5 bool = a__0 <= b__1
    var t29 string = bool_to_string(less_eq1__5)
    var t30 string = "10 <= 20: " + t29
    string_println(t30)
    var less_eq2__6 bool = a__0 <= c__2
    var t31 string = bool_to_string(less_eq2__6)
    var t32 string = "10 <= 10: " + t31
    string_println(t32)
    var greater_eq1__7 bool = b__1 >= a__0
    var t33 string = bool_to_string(greater_eq1__7)
    var t34 string = "20 >= 10: " + t33
    string_println(t34)
    var greater_eq2__8 bool = c__2 >= a__0
    var t35 string = bool_to_string(greater_eq2__8)
    var t36 string = "10 >= 10: " + t35
    string_println(t36)
    var eq1__9 bool = a__0 == c__2
    var t37 string = bool_to_string(eq1__9)
    var t38 string = "10 == 10: " + t37
    string_println(t38)
    var eq2__10 bool = a__0 == b__1
    var t39 string = bool_to_string(eq2__10)
    var t40 string = "10 == 20: " + t39
    string_println(t40)
    var neq1__11 bool = a__0 != b__1
    var t41 string = bool_to_string(neq1__11)
    var t42 string = "10 != 20: " + t41
    string_println(t42)
    var neq2__12 bool = a__0 != c__2
    var t43 string = bool_to_string(neq2__12)
    var t44 string = "10 != 10: " + t43
    string_println(t44)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t45 string = bool_to_string(less__16)
    var t46 string = "2.71 < 3.14: " + t45
    string_println(t46)
    var greater__17 bool = x__13 > y__14
    var t47 string = bool_to_string(greater__17)
    var t48 string = "3.14 > 2.71: " + t47
    string_println(t48)
    var less_eq1__18 bool = y__14 <= x__13
    var t49 string = bool_to_string(less_eq1__18)
    var t50 string = "2.71 <= 3.14: " + t49
    string_println(t50)
    var less_eq2__19 bool = x__13 <= z__15
    var t51 string = bool_to_string(less_eq2__19)
    var t52 string = "3.14 <= 3.14: " + t51
    string_println(t52)
    var greater_eq1__20 bool = x__13 >= y__14
    var t53 string = bool_to_string(greater_eq1__20)
    var t54 string = "3.14 >= 2.71: " + t53
    string_println(t54)
    var greater_eq2__21 bool = z__15 >= x__13
    var t55 string = bool_to_string(greater_eq2__21)
    var t56 string = "3.14 >= 3.14: " + t55
    string_println(t56)
    var eq1__22 bool = x__13 == z__15
    var t57 string = bool_to_string(eq1__22)
    var t58 string = "3.14 == 3.14: " + t57
    string_println(t58)
    var eq2__23 bool = x__13 == y__14
    var t59 string = bool_to_string(eq2__23)
    var t60 string = "3.14 == 2.71: " + t59
    string_println(t60)
    var neq1__24 bool = x__13 != y__14
    var t61 string = bool_to_string(neq1__24)
    var t62 string = "3.14 != 2.71: " + t61
    string_println(t62)
    var neq2__25 bool = x__13 != z__15
    var t63 string = bool_to_string(neq2__25)
    var t64 string = "3.14 != 3.14: " + t63
    string_println(t64)
    return struct{}{}
}

func main0() struct{} {
    string_println("=== Integer Comparisons ===")
    test_int_comparisons()
    string_println("")
    string_println("=== Float Comparisons ===")
    test_float_comparisons()
    return struct{}{}
}

func main() {
    main0()
}
