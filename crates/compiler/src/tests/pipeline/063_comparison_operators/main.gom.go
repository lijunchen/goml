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
    var ret2117 struct{}
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t2078 string = bool_to_string(less__3)
    var t2077 string = "10 < 20: " + t2078
    string_println(t2077)
    var greater__4 bool = b__1 > a__0
    var t2080 string = bool_to_string(greater__4)
    var t2079 string = "20 > 10: " + t2080
    string_println(t2079)
    var less_eq1__5 bool = a__0 <= b__1
    var t2082 string = bool_to_string(less_eq1__5)
    var t2081 string = "10 <= 20: " + t2082
    string_println(t2081)
    var less_eq2__6 bool = a__0 <= c__2
    var t2084 string = bool_to_string(less_eq2__6)
    var t2083 string = "10 <= 10: " + t2084
    string_println(t2083)
    var greater_eq1__7 bool = b__1 >= a__0
    var t2086 string = bool_to_string(greater_eq1__7)
    var t2085 string = "20 >= 10: " + t2086
    string_println(t2085)
    var greater_eq2__8 bool = c__2 >= a__0
    var t2088 string = bool_to_string(greater_eq2__8)
    var t2087 string = "10 >= 10: " + t2088
    string_println(t2087)
    var eq1__9 bool = a__0 == c__2
    var t2090 string = bool_to_string(eq1__9)
    var t2089 string = "10 == 10: " + t2090
    string_println(t2089)
    var eq2__10 bool = a__0 == b__1
    var t2092 string = bool_to_string(eq2__10)
    var t2091 string = "10 == 20: " + t2092
    string_println(t2091)
    var neq1__11 bool = a__0 != b__1
    var t2094 string = bool_to_string(neq1__11)
    var t2093 string = "10 != 20: " + t2094
    string_println(t2093)
    var neq2__12 bool = a__0 != c__2
    var t2096 string = bool_to_string(neq2__12)
    var t2095 string = "10 != 10: " + t2096
    string_println(t2095)
    ret2117 = struct{}{}
    return ret2117
}

func test_float_comparisons() struct{} {
    var ret2118 struct{}
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t2098 string = bool_to_string(less__16)
    var t2097 string = "2.71 < 3.14: " + t2098
    string_println(t2097)
    var greater__17 bool = x__13 > y__14
    var t2100 string = bool_to_string(greater__17)
    var t2099 string = "3.14 > 2.71: " + t2100
    string_println(t2099)
    var less_eq1__18 bool = y__14 <= x__13
    var t2102 string = bool_to_string(less_eq1__18)
    var t2101 string = "2.71 <= 3.14: " + t2102
    string_println(t2101)
    var less_eq2__19 bool = x__13 <= z__15
    var t2104 string = bool_to_string(less_eq2__19)
    var t2103 string = "3.14 <= 3.14: " + t2104
    string_println(t2103)
    var greater_eq1__20 bool = x__13 >= y__14
    var t2106 string = bool_to_string(greater_eq1__20)
    var t2105 string = "3.14 >= 2.71: " + t2106
    string_println(t2105)
    var greater_eq2__21 bool = z__15 >= x__13
    var t2108 string = bool_to_string(greater_eq2__21)
    var t2107 string = "3.14 >= 3.14: " + t2108
    string_println(t2107)
    var eq1__22 bool = x__13 == z__15
    var t2110 string = bool_to_string(eq1__22)
    var t2109 string = "3.14 == 3.14: " + t2110
    string_println(t2109)
    var eq2__23 bool = x__13 == y__14
    var t2112 string = bool_to_string(eq2__23)
    var t2111 string = "3.14 == 2.71: " + t2112
    string_println(t2111)
    var neq1__24 bool = x__13 != y__14
    var t2114 string = bool_to_string(neq1__24)
    var t2113 string = "3.14 != 2.71: " + t2114
    string_println(t2113)
    var neq2__25 bool = x__13 != z__15
    var t2116 string = bool_to_string(neq2__25)
    var t2115 string = "3.14 != 3.14: " + t2116
    string_println(t2115)
    ret2118 = struct{}{}
    return ret2118
}

func main0() struct{} {
    var ret2119 struct{}
    string_println("=== Integer Comparisons ===")
    test_int_comparisons()
    string_println("")
    string_println("=== Float Comparisons ===")
    test_float_comparisons()
    ret2119 = struct{}{}
    return ret2119
}

func main() {
    main0()
}
