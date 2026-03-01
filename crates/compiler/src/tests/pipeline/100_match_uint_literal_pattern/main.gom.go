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
    var jp2 string
    switch x__0 {
    case 0:
        jp2 = "zero"
    case 1:
        jp2 = "one"
    default:
        jp2 = "other"
    }
    var y__1 string = jp2
    string_println(y__1)
    return struct{}{}
}

func main() {
    main0()
}
