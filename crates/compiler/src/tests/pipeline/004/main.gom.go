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
    var a__0 Tuple2_Color_Color = Tuple2_Color_Color{
        _0: Blue{},
        _1: Blue{},
    }
    var x0 Color = a__0._0
    var x1 Color = a__0._1
    var jp4 bool
    switch x1.(type) {
    case Red:
        var jp6 bool
        switch x0.(type) {
        case Red:
            jp6 = true
        case Green:
            jp6 = false
        case Blue:
            jp6 = false
        default:
            panic("non-exhaustive match")
        }
        jp4 = jp6
        return jp4
    case Green:
        var jp8 bool
        switch x0.(type) {
        case Red:
            jp8 = true
        case Green:
            jp8 = false
        case Blue:
            jp8 = false
        default:
            panic("non-exhaustive match")
        }
        jp4 = jp8
        return jp4
    case Blue:
        var jp10 bool
        switch x0.(type) {
        case Red:
            jp10 = false
        case Green:
            jp10 = false
        case Blue:
            print__T_bool(true)
            jp10 = false
        default:
            panic("non-exhaustive match")
        }
        jp4 = jp10
        return jp4
    default:
        panic("non-exhaustive match")
    }
}

func print__T_bool(value__0 bool) struct{} {
    var t11 string = bool_to_string(value__0)
    var t12 struct{} = string_print(t11)
    return t12
}

func main() {
    main0()
}
