package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var ret69 string
    var t63 bool = x__0 < 0
    if t63 {
        ret69 = "negative"
    } else {
        var t64 bool = 0 < x__0
        if t64 {
            ret69 = "positive"
        } else {
            ret69 = "zero"
        }
    }
    return ret69
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var ret70 string
    var t65 bool = a__1 < b__2
    if t65 {
        var t66 bool = b__2 < c__3
        if t66 {
            ret70 = "ascending"
        } else {
            ret70 = "peak"
        }
    } else {
        var t67 bool = a__1 < c__3
        if t67 {
            ret70 = "valley"
        } else {
            ret70 = "flat"
        }
    }
    return ret70
}

func main0() struct{} {
    var ret71 struct{}
    var t68 int32 = -42
    var first__4 string = classify(t68)
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
    ret71 = struct{}{}
    return ret71
}

func main() {
    main0()
}
