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
    var ret43 int32
    switch s__0 {
    case "hello":
        ret43 = 1
    case "world":
        ret43 = 2
    default:
        ret43 = 3
    }
    return ret43
}

func wildcard_position(s__1 string) int32 {
    var ret44 int32
    ret44 = 4
    return ret44
}

func repeated_string(s__2 string) int32 {
    var ret45 int32
    switch s__2 {
    case "hello":
        ret45 = 6
    default:
        ret45 = 8
    }
    return ret45
}

func main0() struct{} {
    var ret46 struct{}
    var t32 int32 = match_string("hello")
    var t31 string = int32_to_string(t32)
    string_println(t31)
    var t34 int32 = match_string("planet")
    var t33 string = int32_to_string(t34)
    string_println(t33)
    var t36 int32 = wildcard_position("world")
    var t35 string = int32_to_string(t36)
    string_println(t35)
    var t38 int32 = wildcard_position("sun")
    var t37 string = int32_to_string(t38)
    string_println(t37)
    var t40 int32 = repeated_string("hello")
    var t39 string = int32_to_string(t40)
    string_println(t39)
    var t42 int32 = repeated_string("mars")
    var t41 string = int32_to_string(t42)
    ret46 = string_println(t41)
    return ret46
}

func main() {
    main0()
}
