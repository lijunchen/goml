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

func main0() struct{} {
    var value__0 int32
    var text__1 string
    var mtmp0 struct{}
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            value__0 = 42
            text__1 = int32_to_string(value__0)
            string_println(text__1)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
