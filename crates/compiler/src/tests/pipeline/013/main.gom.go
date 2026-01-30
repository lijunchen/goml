package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var ret2 struct{}
    var s__0 string = "abcde"
    println__T_string(s__0)
    print__T_string(s__0)
    ret2 = struct{}{}
    return ret2
}

func println__T_string(value__1 string) struct{} {
    var ret3 struct{}
    ret3 = string_println(value__1)
    return ret3
}

func print__T_string(value__0 string) struct{} {
    var ret4 struct{}
    ret4 = string_print(value__0)
    return ret4
}

func main() {
    main0()
}
