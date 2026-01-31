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
    var ret5 struct{}
    var t2 Color = Blue{}
    var t3 Color = Red{}
    var a__0 Tuple2_Color_Color = Tuple2_Color_Color{
        _0: t2,
        _1: t3,
    }
    var x0 Color = a__0._0
    var x1 Color = a__0._1
    switch x1.(type) {
    case Red:
        switch x0.(type) {
        case Red:
            ret5 = print__T_int32(1)
        case Green:
            ret5 = print__T_int32(3)
        case Blue:
            ret5 = print__T_int32(3)
        }
    case Green:
        switch x0.(type) {
        case Red:
            ret5 = print__T_int32(0)
        case Green:
            ret5 = print__T_int32(3)
        case Blue:
            ret5 = print__T_int32(3)
        }
    case Blue:
        switch x0.(type) {
        case Red:
            ret5 = print__T_int32(3)
        case Green:
            ret5 = print__T_int32(3)
        case Blue:
            ret5 = print__T_int32(2)
        }
    }
    return ret5
}

func print__T_int32(value__0 int32) struct{} {
    var ret6 struct{}
    var t4 string = int32_to_string(value__0)
    ret6 = string_print(t4)
    return ret6
}

func main() {
    main0()
}
