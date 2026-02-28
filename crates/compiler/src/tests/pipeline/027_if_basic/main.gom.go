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

func choose(flag__0 bool, x__1 int32, y__2 int32) int32 {
    var jp3 int32
    if flag__0 {
        jp3 = x__1
    } else {
        jp3 = y__2
    }
    return jp3
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t4 string = int32_to_string(yes__3)
    var t5 string = "yes=" + t4
    string_println(t5)
    var t6 string = int32_to_string(no__4)
    var t7 string = "no=" + t6
    string_println(t7)
    return struct{}{}
}

func main() {
    main0()
}
