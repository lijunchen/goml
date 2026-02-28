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

func match_int(n__0 int32) int32 {
    var jp8 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch n__0 {
            case 0:
                pc = 2
            case 1:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp8
        case 2:
            jp8 = 10
            pc = 1
        case 3:
            jp8 = 20
            pc = 1
        case 4:
            jp8 = 30
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func wildcard_first(n__1 int32) int32 {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return 40
        default:
            panic("invalid pc")
        }
    }
}

func wildcard_middle(n__2 int32) int32 {
    var jp10 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch n__2 {
            case 2:
                pc = 2
            case 3:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp10
        case 2:
            jp10 = 90
            pc = 1
        case 3:
            jp10 = 100
            pc = 1
        case 4:
            jp10 = 100
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func repeated(n__3 int32) int32 {
    var jp12 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch n__3 {
            case 1:
                pc = 2
            default:
                pc = 3
            }
        case 1:
            return jp12
        case 2:
            jp12 = 60
            pc = 1
        case 3:
            jp12 = 80
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t13 int32
    var t14 string
    var mtmp0 struct{}
    var t15 int32
    var t16 string
    var mtmp1 struct{}
    var t17 int32
    var t18 string
    var mtmp2 struct{}
    var t19 int32
    var t20 string
    var mtmp3 struct{}
    var t21 int32
    var t22 string
    var mtmp4 struct{}
    var t23 int32
    var t24 string
    var mtmp5 struct{}
    var t25 int32
    var t26 string
    var mtmp6 struct{}
    var t27 int32
    var t28 string
    var t29 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    _ = mtmp6
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t13 = match_int(0)
            t14 = int32_to_string(t13)
            string_println(t14)
            t15 = match_int(5)
            t16 = int32_to_string(t15)
            string_println(t16)
            t17 = wildcard_first(0)
            t18 = int32_to_string(t17)
            string_println(t18)
            t19 = wildcard_first(2)
            t20 = int32_to_string(t19)
            string_println(t20)
            t21 = wildcard_middle(2)
            t22 = int32_to_string(t21)
            string_println(t22)
            t23 = wildcard_middle(3)
            t24 = int32_to_string(t23)
            string_println(t24)
            t25 = repeated(1)
            t26 = int32_to_string(t25)
            string_println(t26)
            t27 = repeated(3)
            t28 = int32_to_string(t27)
            t29 = string_println(t28)
            return t29
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
