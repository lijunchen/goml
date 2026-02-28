package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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

func main0() bool {
    var a__0 Tuple2_Color_Color
    var x0 Color
    var x1 Color
    var jp4 bool
    var jp6 bool
    var jp8 bool
    var jp10 bool
    var mtmp2 struct{}
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__0 = Tuple2_Color_Color{
                _0: Blue{},
                _1: Blue{},
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
            return jp4
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
            jp4 = jp6
            pc = 1
        case 4:
            jp6 = true
            pc = 3
        case 5:
            jp6 = false
            pc = 3
        case 6:
            jp6 = false
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
            jp4 = jp8
            pc = 1
        case 9:
            jp8 = true
            pc = 8
        case 10:
            jp8 = false
            pc = 8
        case 11:
            jp8 = false
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
            jp4 = jp10
            pc = 1
        case 14:
            jp10 = false
            pc = 13
        case 15:
            jp10 = false
            pc = 13
        case 16:
            print__T_bool(true)
            jp10 = false
            pc = 13
        default:
            panic("invalid pc")
        }
    }
}

func print__T_bool(value__0 bool) struct{} {
    var t11 string
    var t12 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = bool_to_string(value__0)
            t12 = string_print(t11)
            return t12
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
