package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var a__0 Tuple2_bool_bool
    var x0 bool
    var x1 bool
    var t4 string
    var t5 struct{}
    var t6 string
    var t7 struct{}
    var t9 string
    var t10 struct{}
    var t11 string
    var t12 struct{}
    _ = t5
    _ = t7
    _ = t10
    _ = t12
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__0 = Tuple2_bool_bool{
                _0: true,
                _1: true,
            }
            x0 = a__0._0
            x1 = a__0._1
            switch x1 {
            case true:
                pc = 2
            case false:
                pc = 6
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            switch x0 {
            case true:
                pc = 4
            case false:
                pc = 5
            default:
                panic("non-exhaustive match")
            }
        case 3:
            pc = 1
        case 4:
            t4 = int32_to_string(789)
            string_print(t4)
            pc = 3
        case 5:
            t6 = int32_to_string(456)
            string_print(t6)
            pc = 3
        case 6:
            switch x0 {
            case true:
                pc = 8
            case false:
                pc = 9
            default:
                panic("non-exhaustive match")
            }
        case 7:
            pc = 1
        case 8:
            t9 = int32_to_string(123)
            string_print(t9)
            pc = 7
        case 9:
            t11 = int32_to_string(789)
            string_print(t11)
            pc = 7
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
