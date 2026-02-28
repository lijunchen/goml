package main

import (
    "fmt"
)

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var s__0 string
    var _wild0 struct{}
    var _wild1 struct{}
    _ = _wild0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            s__0 = "abcde"
            println__T_string(s__0)
            print__T_string(s__0)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t2 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t2 = string_println(value__1)
            return t2
        default:
            panic("invalid pc")
        }
    }
}

func print__T_string(value__0 string) struct{} {
    var t3 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t3 = string_print(value__0)
            return t3
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
