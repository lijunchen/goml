package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

func classify(n__0 int32) string {
    var retv5 string
    var jp7 string
    switch n__0 {
    case -1:
        jp7 = "minus one"
    case 0:
        jp7 = "zero"
    case 1:
        jp7 = "one"
    default:
        jp7 = "other"
    }
    retv5 = jp7
    return retv5
}

func main0() struct{} {
    var t9 string = classify(-1)
    println__T_string(t9)
    var t10 string = classify(0)
    println__T_string(t10)
    var t11 string = classify(1)
    println__T_string(t11)
    var t12 string = classify(42)
    println__T_string(t12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
