package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

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
    string_println(y__1)
    return struct{}{}
}

func main() {
    main0()
}
