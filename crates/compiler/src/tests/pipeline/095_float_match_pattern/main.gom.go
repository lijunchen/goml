package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv6 string
    var jp8 string
    switch x__0 {
    case 0:
        jp8 = "zero"
    case 1:
        jp8 = "one"
    case -1:
        jp8 = "minus one"
    case 3.14:
        jp8 = "pi"
    default:
        jp8 = "other"
    }
    retv6 = jp8
    return retv6
}

func main0() struct{} {
    var t10 string = classify(0)
    println__T_string(t10)
    var t11 string = classify(1)
    println__T_string(t11)
    var t12 float64 = -1
    var t13 string = classify(t12)
    println__T_string(t13)
    var t14 string = classify(3.14)
    println__T_string(t14)
    var t15 string = classify(42)
    println__T_string(t15)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
