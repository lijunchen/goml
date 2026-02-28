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

func choose(flag__0 bool, x__1 int32, y__2 int32) int32 {
    var jp3 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            if flag__0 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp3
        case 2:
            jp3 = x__1
            pc = 1
        case 3:
            jp3 = y__2
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var yes__3 int32
    var no__4 int32
    var t4 string
    var t5 string
    var mtmp0 struct{}
    var t6 string
    var t7 string
    var mtmp1 struct{}
    _ = mtmp0
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            yes__3 = choose(true, 10, 99)
            no__4 = choose(false, 10, 99)
            t4 = int32_to_string(yes__3)
            t5 = "yes=" + t4
            string_println(t5)
            t6 = int32_to_string(no__4)
            t7 = "no=" + t6
            string_println(t7)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
