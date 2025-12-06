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
    var ret7 int32
    var mtmp0 bool = x__0 < 2
    switch mtmp0 {
    case true:
        ret7 = 1
    case false:
        var t2 int32 = x__0 - 1
        var t1 int32 = fib(t2)
        var t4 int32 = x__0 - 2
        var t3 int32 = fib(t4)
        ret7 = t1 + t3
    }
    return ret7
}

func main0() struct{} {
    var ret8 struct{}
    var t6 int32 = fib(10)
    var t5 string = int32_to_string(t6)
    ret8 = string_print(t5)
    return ret8
}

func main() {
    main0()
}
