package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    var x__0 uint8 = 5
    var jp3 string
    switch x__0 {
    case 0:
        jp3 = "zero"
    case 1:
        jp3 = "one"
    default:
        jp3 = "other"
    }
    var y__1 string = jp3
    println__T_string(y__1)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
