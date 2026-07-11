package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Tuple2_5Color_5Color struct {
    _0 Color
    _1 Color
}

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() bool {
    var retv11 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x7 Color = a__0._0
    var x8 Color = a__0._1
    var jp13 bool
    switch x8 {
    case Red:
        var jp15 bool
        switch x7 {
        case Red:
            jp15 = true
        case Green:
            jp15 = false
        case Blue:
            jp15 = false
        default:
            panic("non-exhaustive match")
        }
        jp13 = jp15
    case Green:
        var jp17 bool
        switch x7 {
        case Red:
            jp17 = true
        case Green:
            jp17 = false
        case Blue:
            jp17 = false
        default:
            panic("non-exhaustive match")
        }
        jp13 = jp17
    case Blue:
        var jp19 bool
        switch x7 {
        case Red:
            jp19 = false
        case Green:
            jp19 = false
        case Blue:
            print__T_bool(true)
            jp19 = false
        default:
            panic("non-exhaustive match")
        }
        jp13 = jp19
    default:
        panic("non-exhaustive match")
    }
    retv11 = jp13
    return retv11
}

func print__T_bool(value__0 bool) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv24 string
    var t25 string = _goml_runtime_core_bool_to_string(self__8)
    retv24 = t25
    return retv24
}

func main() {
    main0()
}
