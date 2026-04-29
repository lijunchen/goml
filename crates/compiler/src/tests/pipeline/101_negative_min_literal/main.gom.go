package main

import (
    _goml_fmt "fmt"
)

func int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var a__0 int8 = -128
    var t3 string = int8_to_string(a__0)
    println__T_string(t3)
    var b__1 int16 = -32768
    var t4 string = int16_to_string(b__1)
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
