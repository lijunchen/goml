package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func my_int_equal(x__0 int32, y__1 int32) bool {
    var t0 bool
    var t1 bool
    var t2 bool
    var t3 bool
    var t4 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t0 = x__0 < y__1
            t1 = !t0
            t2 = y__1 < x__0
            t3 = !t2
            t4 = t1 && t3
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func sum(n__2 int32) int32 {
    var t7 bool
    var jp6 int32
    var t8 int32
    var t9 int32
    var t10 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t7 = my_int_equal(n__2, 1)
            if t7 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp6
        case 2:
            jp6 = 1
            pc = 1
        case 3:
            t8 = n__2 - 1
            t9 = sum(t8)
            t10 = n__2 + t9
            jp6 = t10
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t11 int32
    var t12 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = sum(100)
            t12 = println__T_int32(t11)
            return t12
        default:
            panic("invalid pc")
        }
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t13 string
    var t14 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t13 = int32_to_string(value__1)
            t14 = string_println(t13)
            return t14
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
