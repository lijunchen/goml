package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var x__0 int32 = 1
    var t3 int32 = x__0 + 1
    x__0 = t3
    var t4 string = int32_to_string(x__0)
    println__T_string(t4)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
