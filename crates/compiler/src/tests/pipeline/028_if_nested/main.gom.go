package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv7 string
    var t10 bool = x__0 < 0
    var jp9 string
    if t10 {
        jp9 = "negative"
    } else {
        var t13 bool = 0 < x__0
        var jp12 string
        if t13 {
            jp12 = "positive"
        } else {
            jp12 = "zero"
        }
        jp9 = jp12
    }
    retv7 = jp9
    return retv7
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv15 string
    var t18 bool = a__1 < b__2
    var jp17 string
    if t18 {
        var t21 bool = b__2 < c__3
        var jp20 string
        if t21 {
            jp20 = "ascending"
        } else {
            jp20 = "peak"
        }
        jp17 = jp20
    } else {
        var t24 bool = a__1 < c__3
        var jp23 string
        if t24 {
            jp23 = "valley"
        } else {
            jp23 = "flat"
        }
        jp17 = jp23
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var first__4 string = classify(-42)
    var second__5 string = classify(0)
    var third__6 string = classify(17)
    var shape1__7 string = triangle_type(1, 2, 3)
    var shape2__8 string = triangle_type(3, 2, 1)
    var shape3__9 string = triangle_type(2, 3, 2)
    println__T_string(first__4)
    println__T_string(second__5)
    println__T_string(third__6)
    println__T_string(shape1__7)
    println__T_string(shape2__8)
    println__T_string(shape3__9)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
