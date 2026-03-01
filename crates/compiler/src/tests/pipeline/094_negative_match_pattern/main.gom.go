package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var jp5 string
    switch n__0 {
    case -1:
        jp5 = "minus one"
    case 0:
        jp5 = "zero"
    case 1:
        jp5 = "one"
    default:
        jp5 = "other"
    }
    return jp5
}

func main0() struct{} {
    var t6 string = classify(-1)
    println__T_string(t6)
    var t7 string = classify(0)
    println__T_string(t7)
    var t8 string = classify(1)
    println__T_string(t8)
    var t9 string = classify(42)
    println__T_string(t9)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t10 struct{} = string_println(value__1)
    return t10
}

func main() {
    main0()
}
