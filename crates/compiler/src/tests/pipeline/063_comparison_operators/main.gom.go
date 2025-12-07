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
    var ret65 struct{}
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t26 string = bool_to_string(less__3)
    var t25 string = "10 < 20: " + t26
    string_println(t25)
    var greater__4 bool = b__1 > a__0
    var t28 string = bool_to_string(greater__4)
    var t27 string = "20 > 10: " + t28
    string_println(t27)
    var less_eq1__5 bool = a__0 <= b__1
    var t30 string = bool_to_string(less_eq1__5)
    var t29 string = "10 <= 20: " + t30
    string_println(t29)
    var less_eq2__6 bool = a__0 <= c__2
    var t32 string = bool_to_string(less_eq2__6)
    var t31 string = "10 <= 10: " + t32
    string_println(t31)
    var greater_eq1__7 bool = b__1 >= a__0
    var t34 string = bool_to_string(greater_eq1__7)
    var t33 string = "20 >= 10: " + t34
    string_println(t33)
    var greater_eq2__8 bool = c__2 >= a__0
    var t36 string = bool_to_string(greater_eq2__8)
    var t35 string = "10 >= 10: " + t36
    string_println(t35)
    var eq1__9 bool = a__0 == c__2
    var t38 string = bool_to_string(eq1__9)
    var t37 string = "10 == 10: " + t38
    string_println(t37)
    var eq2__10 bool = a__0 == b__1
    var t40 string = bool_to_string(eq2__10)
    var t39 string = "10 == 20: " + t40
    string_println(t39)
    var neq1__11 bool = a__0 != b__1
    var t42 string = bool_to_string(neq1__11)
    var t41 string = "10 != 20: " + t42
    string_println(t41)
    var neq2__12 bool = a__0 != c__2
    var t44 string = bool_to_string(neq2__12)
    var t43 string = "10 != 10: " + t44
    string_println(t43)
    ret65 = struct{}{}
    return ret65
}

func test_float_comparisons() struct{} {
    var ret66 struct{}
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t46 string = bool_to_string(less__16)
    var t45 string = "2.71 < 3.14: " + t46
    string_println(t45)
    var greater__17 bool = x__13 > y__14
    var t48 string = bool_to_string(greater__17)
    var t47 string = "3.14 > 2.71: " + t48
    string_println(t47)
    var less_eq1__18 bool = y__14 <= x__13
    var t50 string = bool_to_string(less_eq1__18)
    var t49 string = "2.71 <= 3.14: " + t50
    string_println(t49)
    var less_eq2__19 bool = x__13 <= z__15
    var t52 string = bool_to_string(less_eq2__19)
    var t51 string = "3.14 <= 3.14: " + t52
    string_println(t51)
    var greater_eq1__20 bool = x__13 >= y__14
    var t54 string = bool_to_string(greater_eq1__20)
    var t53 string = "3.14 >= 2.71: " + t54
    string_println(t53)
    var greater_eq2__21 bool = z__15 >= x__13
    var t56 string = bool_to_string(greater_eq2__21)
    var t55 string = "3.14 >= 3.14: " + t56
    string_println(t55)
    var eq1__22 bool = x__13 == z__15
    var t58 string = bool_to_string(eq1__22)
    var t57 string = "3.14 == 3.14: " + t58
    string_println(t57)
    var eq2__23 bool = x__13 == y__14
    var t60 string = bool_to_string(eq2__23)
    var t59 string = "3.14 == 2.71: " + t60
    string_println(t59)
    var neq1__24 bool = x__13 != y__14
    var t62 string = bool_to_string(neq1__24)
    var t61 string = "3.14 != 2.71: " + t62
    string_println(t61)
    var neq2__25 bool = x__13 != z__15
    var t64 string = bool_to_string(neq2__25)
    var t63 string = "3.14 != 3.14: " + t64
    string_println(t63)
    ret66 = struct{}{}
    return ret66
}

func main0() struct{} {
    var ret67 struct{}
    string_println("=== Integer Comparisons ===")
    test_int_comparisons()
    string_println("")
    string_println("=== Float Comparisons ===")
    test_float_comparisons()
    ret67 = struct{}{}
    return ret67
}

func main() {
    main0()
}
