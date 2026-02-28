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
    a__0 = Tuple2_Color_Color{
        _0: Blue{},
        _1: Blue{},
    }
    x0 = a__0._0
    x1 = a__0._1
    switch x1.(type) {
    case Red:
        goto b2
    case Green:
        goto b7
    case Blue:
        goto b12
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp4
    b2:
    switch x0.(type) {
    case Red:
        goto b4
    case Green:
        goto b5
    case Blue:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b3:
    jp4 = jp6
    goto b1
    b4:
    jp6 = true
    goto b3
    b5:
    jp6 = false
    goto b3
    b6:
    jp6 = false
    goto b3
    b7:
    switch x0.(type) {
    case Red:
        goto b9
    case Green:
        goto b10
    case Blue:
        goto b11
    default:
        panic("non-exhaustive match")
    }
    b8:
    jp4 = jp8
    goto b1
    b9:
    jp8 = true
    goto b8
    b10:
    jp8 = false
    goto b8
    b11:
    jp8 = false
    goto b8
    b12:
    switch x0.(type) {
    case Red:
        goto b14
    case Green:
        goto b15
    case Blue:
        goto b16
    default:
        panic("non-exhaustive match")
    }
    b13:
    jp4 = jp10
    goto b1
    b14:
    jp10 = false
    goto b13
    b15:
    jp10 = false
    goto b13
    b16:
    print__T_bool(true)
    jp10 = false
    goto b13
}

func print__T_bool(value__0 bool) struct{} {
    var t11 string
    var t12 struct{}
    t11 = bool_to_string(value__0)
    t12 = string_print(t11)
    return t12
}

func main() {
    main0()
}
