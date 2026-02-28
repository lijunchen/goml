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
        jp6 = 1
    case "world":
        jp6 = 2
    default:
        jp6 = 3
    }
    return jp6
}

func wildcard_position(s__1 string) int32 {
    return 4
}

func repeated_string(s__2 string) int32 {
    var jp8 int32
    switch s__2 {
    case "hello":
        jp8 = 6
    default:
        jp8 = 8
    }
    return jp8
}

func main0() struct{} {
    var t9 int32 = match_string("hello")
    var t10 string = int32_to_string(t9)
    string_println(t10)
    var t11 int32 = match_string("planet")
    var t12 string = int32_to_string(t11)
    string_println(t12)
    var t13 int32 = wildcard_position("world")
    var t14 string = int32_to_string(t13)
    string_println(t14)
    var t15 int32 = wildcard_position("sun")
    var t16 string = int32_to_string(t15)
    string_println(t16)
    var t17 int32 = repeated_string("hello")
    var t18 string = int32_to_string(t17)
    string_println(t18)
    var t19 int32 = repeated_string("mars")
    var t20 string = int32_to_string(t19)
    var t21 struct{} = string_println(t20)
    return t21
}

func main() {
    main0()
}
