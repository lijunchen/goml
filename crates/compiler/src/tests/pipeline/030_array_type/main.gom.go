package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Buffer struct {
    values [3]int32
}

func main0() struct{} {
    var t0 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t0 = print__T_string("array")
            return t0
        default:
            panic("invalid pc")
        }
    }
}

func print__T_string(value__0 string) struct{} {
    var t1 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t1 = string_print(value__0)
            return t1
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
