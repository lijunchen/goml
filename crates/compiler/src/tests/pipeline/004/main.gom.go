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
    var retv62 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x58 Color = a__0._0
    var x59 Color = a__0._1
    var jp64 bool
    switch x59 {
    case Red:
        var jp66 bool
        switch x58 {
        case Red:
            jp66 = true
        case Green:
            jp66 = false
        case Blue:
            jp66 = false
        default:
            panic("non-exhaustive match")
        }
        jp64 = jp66
    case Green:
        var jp68 bool
        switch x58 {
        case Red:
            jp68 = true
        case Green:
            jp68 = false
        case Blue:
            jp68 = false
        default:
            panic("non-exhaustive match")
        }
        jp64 = jp68
    case Blue:
        var jp70 bool
        switch x58 {
        case Red:
            jp70 = false
        case Green:
            jp70 = false
        case Blue:
            print__T_bool(true)
            jp70 = false
        default:
            panic("non-exhaustive match")
        }
        jp64 = jp70
    default:
        panic("non-exhaustive match")
    }
    retv62 = jp64
    return retv62
}

func print__T_bool(value__0 bool) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv75 string
    var t76 string = _goml_runtime_core_bool_to_string(self__33)
    retv75 = t76
    return retv75
}

func main() {
    main0()
}
