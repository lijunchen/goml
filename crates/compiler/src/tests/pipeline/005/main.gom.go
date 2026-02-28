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
    a__0 = Tuple2_Color_Color{
        _0: Blue{},
        _1: Red{},
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
    return struct{}{}
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
    goto b1
    b4:
    print__T_int32(1)
    goto b3
    b5:
    print__T_int32(3)
    goto b3
    b6:
    print__T_int32(3)
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
    goto b1
    b9:
    print__T_int32(0)
    goto b8
    b10:
    print__T_int32(3)
    goto b8
    b11:
    print__T_int32(3)
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
    goto b1
    b14:
    print__T_int32(3)
    goto b13
    b15:
    print__T_int32(3)
    goto b13
    b16:
    print__T_int32(2)
    goto b13
}

func print__T_int32(value__0 int32) struct{} {
    var t15 string
    var t16 struct{}
    t15 = int32_to_string(value__0)
    t16 = string_print(t15)
    return t16
}

func main() {
    main0()
}
