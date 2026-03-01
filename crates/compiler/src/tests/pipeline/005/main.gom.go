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
    var a__0 Tuple2_Color_Color = Tuple2_Color_Color{
        _0: Blue{},
        _1: Red{},
    }
    var x0 Color = a__0._0
    var x1 Color = a__0._1
    switch x1.(type) {
    case Red:
        switch x0.(type) {
        case Red:
            print__T_int32(1)
        case Green:
            print__T_int32(3)
        case Blue:
            print__T_int32(3)
        default:
            panic("non-exhaustive match")
        }
        return struct{}{}
    case Green:
        switch x0.(type) {
        case Red:
            print__T_int32(0)
        case Green:
            print__T_int32(3)
        case Blue:
            print__T_int32(3)
        default:
            panic("non-exhaustive match")
        }
        return struct{}{}
    case Blue:
        switch x0.(type) {
        case Red:
            print__T_int32(3)
        case Green:
            print__T_int32(3)
        case Blue:
            print__T_int32(2)
        default:
            panic("non-exhaustive match")
        }
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func print__T_int32(value__0 int32) struct{} {
    var t17 string = int32_to_string(value__0)
    string_print(t17)
    return struct{}{}
}

func main() {
    main0()
}
