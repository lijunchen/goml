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

func match_string(s__0 string) int32 {
    var jp6 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch s__0 {
            case "hello":
                pc = 2
            case "world":
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp6
        case 2:
            jp6 = 1
            pc = 1
        case 3:
            jp6 = 2
            pc = 1
        case 4:
            jp6 = 3
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func wildcard_position(s__1 string) int32 {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return 4
        default:
            panic("invalid pc")
        }
    }
}

func repeated_string(s__2 string) int32 {
    var jp8 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch s__2 {
            case "hello":
                pc = 2
            default:
                pc = 3
            }
        case 1:
            return jp8
        case 2:
            jp8 = 6
            pc = 1
        case 3:
            jp8 = 8
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t9 int32
    var t10 string
    var mtmp0 struct{}
    var t11 int32
    var t12 string
    var mtmp1 struct{}
    var t13 int32
    var t14 string
    var mtmp2 struct{}
    var t15 int32
    var t16 string
    var mtmp3 struct{}
    var t17 int32
    var t18 string
    var mtmp4 struct{}
    var t19 int32
    var t20 string
    var t21 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t9 = match_string("hello")
            t10 = int32_to_string(t9)
            string_println(t10)
            t11 = match_string("planet")
            t12 = int32_to_string(t11)
            string_println(t12)
            t13 = wildcard_position("world")
            t14 = int32_to_string(t13)
            string_println(t14)
            t15 = wildcard_position("sun")
            t16 = int32_to_string(t15)
            string_println(t16)
            t17 = repeated_string("hello")
            t18 = int32_to_string(t17)
            string_println(t18)
            t19 = repeated_string("mars")
            t20 = int32_to_string(t19)
            t21 = string_println(t20)
            return t21
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
