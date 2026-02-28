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
        goto b2
    case 1:
        goto b3
    default:
        goto b4
    }
    b1:
    return jp8
    b2:
    jp8 = 10
    goto b1
    b3:
    jp8 = 20
    goto b1
    b4:
    jp8 = 30
    goto b1
}

func wildcard_first(n__1 int32) int32 {
    return 40
}

func wildcard_middle(n__2 int32) int32 {
    var jp10 int32
    switch n__2 {
    case 2:
        goto b2
    case 3:
        goto b3
    default:
        goto b4
    }
    b1:
    return jp10
    b2:
    jp10 = 90
    goto b1
    b3:
    jp10 = 100
    goto b1
    b4:
    jp10 = 100
    goto b1
}

func repeated(n__3 int32) int32 {
    var jp12 int32
    switch n__3 {
    case 1:
        goto b2
    default:
        goto b3
    }
    b1:
    return jp12
    b2:
    jp12 = 60
    goto b1
    b3:
    jp12 = 80
    goto b1
}

func main0() struct{} {
    var t13 int32
    var t14 string
    var t15 int32
    var t16 string
    var t17 int32
    var t18 string
    var t19 int32
    var t20 string
    var t21 int32
    var t22 string
    var t23 int32
    var t24 string
    var t25 int32
    var t26 string
    var t27 int32
    var t28 string
    var t29 struct{}
    t13 = match_int(0)
    t14 = int32_to_string(t13)
    string_println(t14)
    t15 = match_int(5)
    t16 = int32_to_string(t15)
    string_println(t16)
    t17 = wildcard_first(0)
    t18 = int32_to_string(t17)
    string_println(t18)
    t19 = wildcard_first(2)
    t20 = int32_to_string(t19)
    string_println(t20)
    t21 = wildcard_middle(2)
    t22 = int32_to_string(t21)
    string_println(t22)
    t23 = wildcard_middle(3)
    t24 = int32_to_string(t23)
    string_println(t24)
    t25 = repeated(1)
    t26 = int32_to_string(t25)
    string_println(t26)
    t27 = repeated(3)
    t28 = int32_to_string(t27)
    t29 = string_println(t28)
    return t29
}

func main() {
    main0()
}
