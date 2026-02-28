package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func fib(x__0 int32) int32 {
    var mtmp0 bool
    var jp3 int32
    var t4 int32
    var t5 int32
    var t6 int32
    var t7 int32
    var t8 int32
    mtmp0 = x__0 < 2
    switch mtmp0 {
    case true:
        goto b2
    case false:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp3
    b2:
    jp3 = 1
    goto b1
    b3:
    t4 = x__0 - 1
    t5 = fib(t4)
    t6 = x__0 - 2
    t7 = fib(t6)
    t8 = t5 + t7
    jp3 = t8
    goto b1
}

func main0() struct{} {
    var t9 int32
    t9 = fib(10)
    print__T_int32(t9)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t10 string
    var t11 struct{}
    t10 = int32_to_string(value__0)
    t11 = string_print(t10)
    return t11
}

func main() {
    main0()
}
