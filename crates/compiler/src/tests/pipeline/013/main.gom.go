package main

import (
    _goml_fmt "fmt"
)

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var s__0 string = "abcde"
    println__T_string(s__0)
    print__T_string(s__0)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func main() {
    main0()
}
