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

type _goml_Lib_x3a__x3a_Color interface {
    is_goml_Lib_x3a__x3a_Color()
}

type Red struct {}

func (_ Red) is_goml_Lib_x3a__x3a_Color() {}

type Green struct {}

func (_ Green) is_goml_Lib_x3a__x3a_Color() {}

func main0() struct{} {
    var t0 int32
    var t1 string
    var t2 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t0 = _goml_Lib_x3a__x3a_color_to_int(Red{})
            t1 = int32_to_string(t0)
            t2 = string_println(t1)
            return t2
        default:
            panic("invalid pc")
        }
    }
}

func _goml_Lib_x3a__x3a_color_to_int(c__0 _goml_Lib_x3a__x3a_Color) int32 {
    var jp4 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch c__0.(type) {
            case Red:
                pc = 2
            case Green:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp4
        case 2:
            jp4 = 1
            pc = 1
        case 3:
            jp4 = 2
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
