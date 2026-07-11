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
    var retv8 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x4 Color = a__0._0
    var x5 Color = a__0._1
    var jp10 bool
    switch x5 {
    case Red:
        var jp12 bool
        switch x4 {
        case Red:
            jp12 = true
        case Green:
            jp12 = false
        case Blue:
            jp12 = false
        default:
            panic("non-exhaustive match")
        }
        jp10 = jp12
    case Green:
        var jp14 bool
        switch x4 {
        case Red:
            jp14 = true
        case Green:
            jp14 = false
        case Blue:
            jp14 = false
        default:
            panic("non-exhaustive match")
        }
        jp10 = jp14
    case Blue:
        var jp16 bool
        switch x4 {
        case Red:
            jp16 = false
        case Green:
            jp16 = false
        case Blue:
            print__T_bool(true)
            jp16 = false
        default:
            panic("non-exhaustive match")
        }
        jp10 = jp16
    default:
        panic("non-exhaustive match")
    }
    retv8 = jp10
    return retv8
}

func print__T_bool(value__0 bool) struct{} {
    var t18 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t18)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv21 string
    var t22 string = _goml_runtime_core_bool_to_string(self__8)
    retv21 = t22
    return retv21
}

func main() {
    main0()
}
