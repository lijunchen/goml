package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var t3 [3]int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t3 = [3]int32{1, 2, 3}
            return t3
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var arr__0 [3]int32
    var inline__1 [3]int32
    var mtmp0 struct{}
    var mtmp1 [3]int32
    var mtmp2 [3]int32
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            arr__0 = make_array()
            inline__1 = [3]int32{4, 5, 6}
            print__T_string("array literal")
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func print__T_string(value__0 string) struct{} {
    var t4 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = string_print(value__0)
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
