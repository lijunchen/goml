package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func _goml_Lib_x3a__x3a_msg() string {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "hi"
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t0 string
    var t1 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t0 = _goml_Lib_x3a__x3a_msg()
            t1 = string_println(t0)
            return t1
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
