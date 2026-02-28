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
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t8 = x__0 < 0
            if t8 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp7
        case 2:
            jp7 = "negative"
            pc = 1
        case 3:
            t11 = 0 < x__0
            if t11 {
                pc = 5
            } else {
                pc = 6
            }
        case 4:
            jp7 = jp10
            pc = 1
        case 5:
            jp10 = "positive"
            pc = 4
        case 6:
            jp10 = "zero"
            pc = 4
        default:
            panic("invalid pc")
        }
    }
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var t14 bool
    var jp13 string
    var t17 bool
    var jp16 string
    var t20 bool
    var jp19 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t14 = a__1 < b__2
            if t14 {
                pc = 2
            } else {
                pc = 6
            }
        case 1:
            return jp13
        case 2:
            t17 = b__2 < c__3
            if t17 {
                pc = 4
            } else {
                pc = 5
            }
        case 3:
            jp13 = jp16
            pc = 1
        case 4:
            jp16 = "ascending"
            pc = 3
        case 5:
            jp16 = "peak"
            pc = 3
        case 6:
            t20 = a__1 < c__3
            if t20 {
                pc = 8
            } else {
                pc = 9
            }
        case 7:
            jp13 = jp19
            pc = 1
        case 8:
            jp19 = "valley"
            pc = 7
        case 9:
            jp19 = "flat"
            pc = 7
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t21 int32
    var first__4 string
    var second__5 string
    var third__6 string
    var shape1__7 string
    var shape2__8 string
    var shape3__9 string
    var mtmp0 struct{}
    var mtmp1 struct{}
    var mtmp2 struct{}
    var mtmp3 struct{}
    var mtmp4 struct{}
    var mtmp5 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    var pc int32 = 0
    for {
        switch pc {
        case 0:
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
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
