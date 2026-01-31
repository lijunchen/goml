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
    var ret6 bool
    var t3 Color = Blue{}
    var t4 Color = Blue{}
    var a__0 Tuple2_Color_Color = Tuple2_Color_Color{
        _0: t3,
        _1: t4,
    }
    var x0 Color = a__0._0
    var x1 Color = a__0._1
    switch x1.(type) {
    case Red:
        switch x0.(type) {
        case Red:
            ret6 = true
        case Green:
            ret6 = false
        case Blue:
            ret6 = false
        }
    case Green:
        switch x0.(type) {
        case Red:
            ret6 = true
        case Green:
            ret6 = false
        case Blue:
            ret6 = false
        }
    case Blue:
        switch x0.(type) {
        case Red:
            ret6 = false
        case Green:
            ret6 = false
        case Blue:
            print__T_bool(true)
            ret6 = false
        }
    }
    return ret6
}

func print__T_bool(value__0 bool) struct{} {
    var ret7 struct{}
    var t5 string = bool_to_string(value__0)
    ret7 = string_print(t5)
    return ret7
}

func main() {
    main0()
}
