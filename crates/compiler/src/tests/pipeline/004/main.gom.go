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
    var retv26 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x22 Color = a__0._0
    var x23 Color = a__0._1
    var jp28 bool
    switch x23 {
    case Red:
        var jp30 bool
        switch x22 {
        case Red:
            jp30 = true
        case Green:
            jp30 = false
        case Blue:
            jp30 = false
        default:
            panic("non-exhaustive match")
        }
        jp28 = jp30
    case Green:
        var jp32 bool
        switch x22 {
        case Red:
            jp32 = true
        case Green:
            jp32 = false
        case Blue:
            jp32 = false
        default:
            panic("non-exhaustive match")
        }
        jp28 = jp32
    case Blue:
        var jp34 bool
        switch x22 {
        case Red:
            jp34 = false
        case Green:
            jp34 = false
        case Blue:
            print__T_bool(true)
            jp34 = false
        default:
            panic("non-exhaustive match")
        }
        jp28 = jp34
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func print__T_bool(value__0 bool) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv39 string
    var t40 string = _goml_runtime_core_bool_to_string(self__8)
    retv39 = t40
    return retv39
}

func main() {
    main0()
}
