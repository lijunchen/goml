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
    var retv112 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x108 Color = a__0._0
    var x109 Color = a__0._1
    var jp114 bool
    switch x109 {
    case Red:
        var jp116 bool
        switch x108 {
        case Red:
            jp116 = true
        default:
            jp116 = false
        }
        jp114 = jp116
    case Green:
        var jp118 bool
        switch x108 {
        case Red:
            jp118 = true
        default:
            jp118 = false
        }
        jp114 = jp118
    case Blue:
        var jp120 bool
        switch x108 {
        case Blue:
            print__T_bool(true)
            jp120 = false
        default:
            jp120 = false
        }
        jp114 = jp120
    default:
        panic("non-exhaustive match")
    }
    retv112 = jp114
    return retv112
}

func print__T_bool(value__0 bool) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv125 string
    var t126 string = _goml_runtime_core_bool_to_string(self__37)
    retv125 = t126
    return retv125
}

func main() {
    main0()
}
