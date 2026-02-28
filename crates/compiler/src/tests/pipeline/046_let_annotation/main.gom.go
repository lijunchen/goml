package main

import (
    "fmt"
)

func int8_to_string(x int8) string {
    return fmt.Sprintf("%d", x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 int32
    var y__1 int8
    var mtmp0 struct{}
    var mtmp1 struct{}
    var mtmp2 struct{}
    var mtmp3 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x__0 = 1
            y__1 = 1
            print__T_string("int32: ")
            println__T_int32(x__0)
            print__T_string("int8: ")
            println__T_int8(y__1)
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

func println__T_int32(value__1 int32) struct{} {
    var t5 string
    var t6 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t5 = int32_to_string(value__1)
            t6 = string_println(t5)
            return t6
        default:
            panic("invalid pc")
        }
    }
}

func println__T_int8(value__1 int8) struct{} {
    var t7 string
    var t8 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t7 = int8_to_string(value__1)
            t8 = string_println(t7)
            return t8
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
