package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type GoError = error

func fib(x__0 int32) int32 {
    var retv3 int32
    var mtmp0 bool = x__0 < 2
    var jp5 int32
    switch mtmp0 {
    case true:
        jp5 = 1
    case false:
        var t6 int32 = x__0 - 1
        var t7 int32 = fib(t6)
        var t8 int32 = x__0 - 2
        var t9 int32 = fib(t8)
        var t10 int32 = t7 + t9
        jp5 = t10
    default:
        panic("non-exhaustive match")
    }
    retv3 = jp5
    return retv3
}

func main0() struct{} {
    var t12 int32 = fib(10)
    print__T_int32(t12)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t14 string = int32_to_string(value__0)
    string_print(t14)
    return struct{}{}
}

func main() {
    main0()
}
