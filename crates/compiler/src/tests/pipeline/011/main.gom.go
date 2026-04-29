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
    var a__0 int32 = 1
    var a__1 int32 = a__0 + 2
    var a__2 int32 = a__1 + 3
    var a__3 int32 = a__2 + 4
    var t1 string = int32_to_string(a__3)
    println__T_string(t1)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
