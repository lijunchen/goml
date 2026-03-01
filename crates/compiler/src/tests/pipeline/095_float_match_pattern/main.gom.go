package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var jp6 string
    switch x__0 {
    case 0:
        jp6 = "zero"
    case 1:
        jp6 = "one"
    case -1:
        jp6 = "minus one"
    case 3.14:
        jp6 = "pi"
    default:
        jp6 = "other"
    }
    return jp6
}

func main0() struct{} {
    var t7 string = classify(0)
    println__T_string(t7)
    var t8 string = classify(1)
    println__T_string(t8)
    var t9 float64 = -1
    var t10 string = classify(t9)
    println__T_string(t10)
    var t11 string = classify(3.14)
    println__T_string(t11)
    var t12 string = classify(42)
    println__T_string(t12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t13 struct{} = string_println(value__1)
    return t13
}

func main() {
    main0()
}
