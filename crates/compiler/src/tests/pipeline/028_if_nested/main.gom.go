package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t8 bool = x__0 < 0
    var jp7 string
    if t8 {
        jp7 = "negative"
        return jp7
    } else {
        var t11 bool = 0 < x__0
        var jp10 string
        if t11 {
            jp10 = "positive"
        } else {
            jp10 = "zero"
        }
        jp7 = jp10
        return jp7
    }
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var t14 bool = a__1 < b__2
    var jp13 string
    if t14 {
        var t17 bool = b__2 < c__3
        var jp16 string
        if t17 {
            jp16 = "ascending"
        } else {
            jp16 = "peak"
        }
        jp13 = jp16
        return jp13
    } else {
        var t20 bool = a__1 < c__3
        var jp19 string
        if t20 {
            jp19 = "valley"
        } else {
            jp19 = "flat"
        }
        jp13 = jp19
        return jp13
    }
}

func main0() struct{} {
    var t21 int32 = -42
    var first__4 string = classify(t21)
    var second__5 string = classify(0)
    var third__6 string = classify(17)
    var shape1__7 string = triangle_type(1, 2, 3)
    var shape2__8 string = triangle_type(3, 2, 1)
    var shape3__9 string = triangle_type(2, 3, 2)
    string_println(first__4)
    string_println(second__5)
    string_println(third__6)
    string_println(shape1__7)
    string_println(shape2__8)
    string_println(shape3__9)
    return struct{}{}
}

func main() {
    main0()
}
