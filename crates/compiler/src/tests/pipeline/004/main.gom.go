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

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

type GoError = error

func main0() bool {
    var retv4 bool
    var a__0 Tuple2_Color_Color = Tuple2_Color_Color{
        _0: Blue,
        _1: Blue,
    }
    var x0 Color = a__0._0
    var x1 Color = a__0._1
    var jp6 bool
    switch x1 {
    case Red:
        var jp8 bool
        switch x0 {
        case Red:
            jp8 = true
        case Green:
            jp8 = false
        case Blue:
            jp8 = false
        default:
            panic("non-exhaustive match")
        }
        jp6 = jp8
        retv4 = jp6
        return retv4
    case Green:
        var jp10 bool
        switch x0 {
        case Red:
            jp10 = true
        case Green:
            jp10 = false
        case Blue:
            jp10 = false
        default:
            panic("non-exhaustive match")
        }
        jp6 = jp10
        retv4 = jp6
        return retv4
    case Blue:
        var jp12 bool
        switch x0 {
        case Red:
            jp12 = false
        case Green:
            jp12 = false
        case Blue:
            print__T_bool(true)
            jp12 = false
        default:
            panic("non-exhaustive match")
        }
        jp6 = jp12
        retv4 = jp6
        return retv4
    default:
        panic("non-exhaustive match")
    }
}

func print__T_bool(value__0 bool) struct{} {
    var t14 string = bool_to_string(value__0)
    string_print(t14)
    return struct{}{}
}

func main() {
    main0()
}
