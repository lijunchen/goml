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
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp3
    b2:
    jp3 = x__1
    goto b1
    b3:
    jp3 = y__2
    goto b1
}

func main0() struct{} {
    var yes__3 int32
    var no__4 int32
    var t4 string
    var t5 string
    var t6 string
    var t7 string
    yes__3 = choose(true, 10, 99)
    no__4 = choose(false, 10, 99)
    t4 = int32_to_string(yes__3)
    t5 = "yes=" + t4
    string_println(t5)
    t6 = int32_to_string(no__4)
    t7 = "no=" + t6
    string_println(t7)
    return struct{}{}
}

func main() {
    main0()
}
