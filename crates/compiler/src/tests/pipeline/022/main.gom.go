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

func match_string(s__0 string) int32 {
    var jp6 int32
    switch s__0 {
    case "hello":
        goto b2
    case "world":
        goto b3
    default:
        goto b4
    }
    b1:
    return jp6
    b2:
    jp6 = 1
    goto b1
    b3:
    jp6 = 2
    goto b1
    b4:
    jp6 = 3
    goto b1
}

func wildcard_position(s__1 string) int32 {
    return 4
}

func repeated_string(s__2 string) int32 {
    var jp8 int32
    switch s__2 {
    case "hello":
        goto b2
    default:
        goto b3
    }
    b1:
    return jp8
    b2:
    jp8 = 6
    goto b1
    b3:
    jp8 = 8
    goto b1
}

func main0() struct{} {
    var t9 int32
    var t10 string
    var t11 int32
    var t12 string
    var t13 int32
    var t14 string
    var t15 int32
    var t16 string
    var t17 int32
    var t18 string
    var t19 int32
    var t20 string
    var t21 struct{}
    t9 = match_string("hello")
    t10 = int32_to_string(t9)
    string_println(t10)
    t11 = match_string("planet")
    t12 = int32_to_string(t11)
    string_println(t12)
    t13 = wildcard_position("world")
    t14 = int32_to_string(t13)
    string_println(t14)
    t15 = wildcard_position("sun")
    t16 = int32_to_string(t15)
    string_println(t16)
    t17 = repeated_string("hello")
    t18 = int32_to_string(t17)
    string_println(t18)
    t19 = repeated_string("mars")
    t20 = int32_to_string(t19)
    t21 = string_println(t20)
    return t21
}

func main() {
    main0()
}
