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
    var a__0 int32
    var b__1 int32
    var c__2 int32
    var less__3 bool
    var t25 string
    var t26 string
    var mtmp0 struct{}
    var greater__4 bool
    var t27 string
    var t28 string
    var mtmp1 struct{}
    var less_eq1__5 bool
    var t29 string
    var t30 string
    var mtmp2 struct{}
    var less_eq2__6 bool
    var t31 string
    var t32 string
    var mtmp3 struct{}
    var greater_eq1__7 bool
    var t33 string
    var t34 string
    var mtmp4 struct{}
    var greater_eq2__8 bool
    var t35 string
    var t36 string
    var mtmp5 struct{}
    var eq1__9 bool
    var t37 string
    var t38 string
    var mtmp6 struct{}
    var eq2__10 bool
    var t39 string
    var t40 string
    var mtmp7 struct{}
    var neq1__11 bool
    var t41 string
    var t42 string
    var mtmp8 struct{}
    var neq2__12 bool
    var t43 string
    var t44 string
    var mtmp9 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    _ = mtmp6
    _ = mtmp7
    _ = mtmp8
    _ = mtmp9
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__0 = 10
            b__1 = 20
            c__2 = 10
            less__3 = a__0 < b__1
            t25 = bool_to_string(less__3)
            t26 = "10 < 20: " + t25
            string_println(t26)
            greater__4 = b__1 > a__0
            t27 = bool_to_string(greater__4)
            t28 = "20 > 10: " + t27
            string_println(t28)
            less_eq1__5 = a__0 <= b__1
            t29 = bool_to_string(less_eq1__5)
            t30 = "10 <= 20: " + t29
            string_println(t30)
            less_eq2__6 = a__0 <= c__2
            t31 = bool_to_string(less_eq2__6)
            t32 = "10 <= 10: " + t31
            string_println(t32)
            greater_eq1__7 = b__1 >= a__0
            t33 = bool_to_string(greater_eq1__7)
            t34 = "20 >= 10: " + t33
            string_println(t34)
            greater_eq2__8 = c__2 >= a__0
            t35 = bool_to_string(greater_eq2__8)
            t36 = "10 >= 10: " + t35
            string_println(t36)
            eq1__9 = a__0 == c__2
            t37 = bool_to_string(eq1__9)
            t38 = "10 == 10: " + t37
            string_println(t38)
            eq2__10 = a__0 == b__1
            t39 = bool_to_string(eq2__10)
            t40 = "10 == 20: " + t39
            string_println(t40)
            neq1__11 = a__0 != b__1
            t41 = bool_to_string(neq1__11)
            t42 = "10 != 20: " + t41
            string_println(t42)
            neq2__12 = a__0 != c__2
            t43 = bool_to_string(neq2__12)
            t44 = "10 != 10: " + t43
            string_println(t44)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func test_float_comparisons() struct{} {
    var x__13 float64
    var y__14 float64
    var z__15 float64
    var less__16 bool
    var t45 string
    var t46 string
    var mtmp10 struct{}
    var greater__17 bool
    var t47 string
    var t48 string
    var mtmp11 struct{}
    var less_eq1__18 bool
    var t49 string
    var t50 string
    var mtmp12 struct{}
    var less_eq2__19 bool
    var t51 string
    var t52 string
    var mtmp13 struct{}
    var greater_eq1__20 bool
    var t53 string
    var t54 string
    var mtmp14 struct{}
    var greater_eq2__21 bool
    var t55 string
    var t56 string
    var mtmp15 struct{}
    var eq1__22 bool
    var t57 string
    var t58 string
    var mtmp16 struct{}
    var eq2__23 bool
    var t59 string
    var t60 string
    var mtmp17 struct{}
    var neq1__24 bool
    var t61 string
    var t62 string
    var mtmp18 struct{}
    var neq2__25 bool
    var t63 string
    var t64 string
    var mtmp19 struct{}
    _ = mtmp10
    _ = mtmp11
    _ = mtmp12
    _ = mtmp13
    _ = mtmp14
    _ = mtmp15
    _ = mtmp16
    _ = mtmp17
    _ = mtmp18
    _ = mtmp19
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x__13 = 3.14
            y__14 = 2.71
            z__15 = 3.14
            less__16 = y__14 < x__13
            t45 = bool_to_string(less__16)
            t46 = "2.71 < 3.14: " + t45
            string_println(t46)
            greater__17 = x__13 > y__14
            t47 = bool_to_string(greater__17)
            t48 = "3.14 > 2.71: " + t47
            string_println(t48)
            less_eq1__18 = y__14 <= x__13
            t49 = bool_to_string(less_eq1__18)
            t50 = "2.71 <= 3.14: " + t49
            string_println(t50)
            less_eq2__19 = x__13 <= z__15
            t51 = bool_to_string(less_eq2__19)
            t52 = "3.14 <= 3.14: " + t51
            string_println(t52)
            greater_eq1__20 = x__13 >= y__14
            t53 = bool_to_string(greater_eq1__20)
            t54 = "3.14 >= 2.71: " + t53
            string_println(t54)
            greater_eq2__21 = z__15 >= x__13
            t55 = bool_to_string(greater_eq2__21)
            t56 = "3.14 >= 3.14: " + t55
            string_println(t56)
            eq1__22 = x__13 == z__15
            t57 = bool_to_string(eq1__22)
            t58 = "3.14 == 3.14: " + t57
            string_println(t58)
            eq2__23 = x__13 == y__14
            t59 = bool_to_string(eq2__23)
            t60 = "3.14 == 2.71: " + t59
            string_println(t60)
            neq1__24 = x__13 != y__14
            t61 = bool_to_string(neq1__24)
            t62 = "3.14 != 2.71: " + t61
            string_println(t62)
            neq2__25 = x__13 != z__15
            t63 = bool_to_string(neq2__25)
            t64 = "3.14 != 3.14: " + t63
            string_println(t64)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var mtmp20 struct{}
    var mtmp21 struct{}
    var mtmp22 struct{}
    var mtmp23 struct{}
    var mtmp24 struct{}
    _ = mtmp20
    _ = mtmp21
    _ = mtmp22
    _ = mtmp23
    _ = mtmp24
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            string_println("=== Integer Comparisons ===")
            test_int_comparisons()
            string_println("")
            string_println("=== Float Comparisons ===")
            test_float_comparisons()
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
