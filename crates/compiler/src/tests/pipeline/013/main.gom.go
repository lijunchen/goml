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
    var s__0 string = "abcde"
    println__T_string(s__0)
    print__T_string(s__0)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t2 struct{} = string_println(value__1)
    return t2
}

func print__T_string(value__0 string) struct{} {
    var t3 struct{} = string_print(value__0)
    return t3
}

func main() {
    main0()
}
