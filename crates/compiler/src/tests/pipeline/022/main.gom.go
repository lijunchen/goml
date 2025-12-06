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
    var ret17 int32
    switch s__0 {
    case "hello":
        ret17 = 1
    case "world":
        ret17 = 2
    default:
        ret17 = 3
    }
    return ret17
}

func wildcard_position(s__1 string) int32 {
    var ret18 int32
    ret18 = 4
    return ret18
}

func repeated_string(s__2 string) int32 {
    var ret19 int32
    switch s__2 {
    case "hello":
        ret19 = 6
    default:
        ret19 = 8
    }
    return ret19
}

func main0() struct{} {
    var ret20 struct{}
    var t6 int32 = match_string("hello")
    var t5 string = int32_to_string(t6)
    string_println(t5)
    var t8 int32 = match_string("planet")
    var t7 string = int32_to_string(t8)
    string_println(t7)
    var t10 int32 = wildcard_position("world")
    var t9 string = int32_to_string(t10)
    string_println(t9)
    var t12 int32 = wildcard_position("sun")
    var t11 string = int32_to_string(t12)
    string_println(t11)
    var t14 int32 = repeated_string("hello")
    var t13 string = int32_to_string(t14)
    string_println(t13)
    var t16 int32 = repeated_string("mars")
    var t15 string = int32_to_string(t16)
    ret20 = string_println(t15)
    return ret20
}

func main() {
    main0()
}
