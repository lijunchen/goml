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

func match_int(n__0 int32) int32 {
    var jp8 int32
    switch n__0 {
    case 0:
        jp8 = 10
    case 1:
        jp8 = 20
    default:
        jp8 = 30
    }
    return jp8
}

func wildcard_first(n__1 int32) int32 {
    return 40
}

func wildcard_middle(n__2 int32) int32 {
    var jp10 int32
    switch n__2 {
    case 2:
        jp10 = 90
    case 3:
        jp10 = 100
    default:
        jp10 = 100
    }
    return jp10
}

func repeated(n__3 int32) int32 {
    var jp12 int32
    switch n__3 {
    case 1:
        jp12 = 60
    default:
        jp12 = 80
    }
    return jp12
}

func main0() struct{} {
    var t13 int32 = match_int(0)
    var t14 string = int32_to_string(t13)
    string_println(t14)
    var t15 int32 = match_int(5)
    var t16 string = int32_to_string(t15)
    string_println(t16)
    var t17 int32 = wildcard_first(0)
    var t18 string = int32_to_string(t17)
    string_println(t18)
    var t19 int32 = wildcard_first(2)
    var t20 string = int32_to_string(t19)
    string_println(t20)
    var t21 int32 = wildcard_middle(2)
    var t22 string = int32_to_string(t21)
    string_println(t22)
    var t23 int32 = wildcard_middle(3)
    var t24 string = int32_to_string(t23)
    string_println(t24)
    var t25 int32 = repeated(1)
    var t26 string = int32_to_string(t25)
    string_println(t26)
    var t27 int32 = repeated(3)
    var t28 string = int32_to_string(t27)
    var t29 struct{} = string_println(t28)
    return t29
}

func main() {
    main0()
}
