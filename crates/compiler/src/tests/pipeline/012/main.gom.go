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
    var mtmp0 bool = x__0 < 2
    var jp3 int32
    switch mtmp0 {
    case true:
        jp3 = 1
    case false:
        var t4 int32 = x__0 - 1
        var t5 int32 = fib(t4)
        var t6 int32 = x__0 - 2
        var t7 int32 = fib(t6)
        var t8 int32 = t5 + t7
        jp3 = t8
    default:
        panic("non-exhaustive match")
    }
    return jp3
}

func main0() struct{} {
    var t9 int32 = fib(10)
    print__T_int32(t9)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t10 string = int32_to_string(value__0)
    var t11 struct{} = string_print(t10)
    return t11
}

func main() {
    main0()
}
