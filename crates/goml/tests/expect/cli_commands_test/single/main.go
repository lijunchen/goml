package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t0 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t0 = string_println("ok")
            return t0
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
