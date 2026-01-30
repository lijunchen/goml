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
    var ret8 int32
    var mtmp0 bool = x__0 < 2
    switch mtmp0 {
    case true:
        ret8 = 1
    case false:
        var t3 int32 = x__0 - 1
        var t2 int32 = fib(t3)
        var t5 int32 = x__0 - 2
        var t4 int32 = fib(t5)
        ret8 = t2 + t4
    }
    return ret8
}

func main0() struct{} {
    var ret9 struct{}
    var t6 int32 = fib(10)
    print__T_int32(t6)
    ret9 = struct{}{}
    return ret9
}

func print__T_int32(value__0 int32) struct{} {
    var ret10 struct{}
    var t7 string = int32_to_string(value__0)
    ret10 = string_print(t7)
    return ret10
}

func main() {
    main0()
}
