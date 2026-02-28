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

type Tuple2_Color_Color struct {
    _0 Color
    _1 Color
}

type Color interface {
    isColor()
}

type Red struct {}

func (_ Red) isColor() {}

type Green struct {}

func (_ Green) isColor() {}

type Blue struct {}

func (_ Blue) isColor() {}

func main0() struct{} {
    var a__0 Tuple2_Color_Color
    var x0 Color
    var x1 Color
    var t4 struct{}
    var t5 struct{}
    var t6 struct{}
    var t8 struct{}
    var t9 struct{}
    var t10 struct{}
    var t12 struct{}
    var t13 struct{}
    var t14 struct{}
    _ = t4
    _ = t5
    _ = t6
    _ = t8
    _ = t9
    _ = t10
    _ = t12
    _ = t13
    _ = t14
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__0 = Tuple2_Color_Color{
                _0: Blue{},
                _1: Red{},
            }
            x0 = a__0._0
            x1 = a__0._1
            switch x1.(type) {
            case Red:
                pc = 2
            case Green:
                pc = 7
            case Blue:
                pc = 12
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            switch x0.(type) {
            case Red:
                pc = 4
            case Green:
                pc = 5
            case Blue:
                pc = 6
            default:
                panic("non-exhaustive match")
            }
        case 3:
            pc = 1
        case 4:
            print__T_int32(1)
            pc = 3
        case 5:
            print__T_int32(3)
            pc = 3
        case 6:
            print__T_int32(3)
            pc = 3
        case 7:
            switch x0.(type) {
            case Red:
                pc = 9
            case Green:
                pc = 10
            case Blue:
                pc = 11
            default:
                panic("non-exhaustive match")
            }
        case 8:
            pc = 1
        case 9:
            print__T_int32(0)
            pc = 8
        case 10:
            print__T_int32(3)
            pc = 8
        case 11:
            print__T_int32(3)
            pc = 8
        case 12:
            switch x0.(type) {
            case Red:
                pc = 14
            case Green:
                pc = 15
            case Blue:
                pc = 16
            default:
                panic("non-exhaustive match")
            }
        case 13:
            pc = 1
        case 14:
            print__T_int32(3)
            pc = 13
        case 15:
            print__T_int32(3)
            pc = 13
        case 16:
            print__T_int32(2)
            pc = 13
        default:
            panic("invalid pc")
        }
    }
}

func print__T_int32(value__0 int32) struct{} {
    var t15 string
    var t16 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t15 = int32_to_string(value__0)
            t16 = string_print(t15)
            return t16
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
