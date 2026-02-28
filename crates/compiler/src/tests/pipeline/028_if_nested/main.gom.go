package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t8 bool
    var jp7 string
    var t11 bool
    var jp10 string
    t8 = x__0 < 0
    if t8 {
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp7
    b2:
    jp7 = "negative"
    goto b1
    b3:
    t11 = 0 < x__0
    if t11 {
        goto b5
    } else {
        goto b6
    }
    b4:
    jp7 = jp10
    goto b1
    b5:
    jp10 = "positive"
    goto b4
    b6:
    jp10 = "zero"
    goto b4
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var t14 bool
    var jp13 string
    var t17 bool
    var jp16 string
    var t20 bool
    var jp19 string
    t14 = a__1 < b__2
    if t14 {
        goto b2
    } else {
        goto b6
    }
    b1:
    return jp13
    b2:
    t17 = b__2 < c__3
    if t17 {
        goto b4
    } else {
        goto b5
    }
    b3:
    jp13 = jp16
    goto b1
    b4:
    jp16 = "ascending"
    goto b3
    b5:
    jp16 = "peak"
    goto b3
    b6:
    t20 = a__1 < c__3
    if t20 {
        goto b8
    } else {
        goto b9
    }
    b7:
    jp13 = jp19
    goto b1
    b8:
    jp19 = "valley"
    goto b7
    b9:
    jp19 = "flat"
    goto b7
}

func main0() struct{} {
    var t21 int32
    var first__4 string
    var second__5 string
    var third__6 string
    var shape1__7 string
    var shape2__8 string
    var shape3__9 string
    t21 = -42
    first__4 = classify(t21)
    second__5 = classify(0)
    third__6 = classify(17)
    shape1__7 = triangle_type(1, 2, 3)
    shape2__8 = triangle_type(3, 2, 1)
    shape3__9 = triangle_type(2, 3, 2)
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
