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
    var retv156 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x152 Color = a__0._0
    var x153 Color = a__0._1
    var jp158 bool
    switch x153 {
    case Red:
        var jp160 bool
        switch x152 {
        case Red:
            jp160 = true
        default:
            jp160 = false
        }
        jp158 = jp160
    case Green:
        var jp162 bool
        switch x152 {
        case Red:
            jp162 = true
        default:
            jp162 = false
        }
        jp158 = jp162
    case Blue:
        var jp164 bool
        switch x152 {
        case Blue:
            print__T_bool(true)
            jp164 = false
        default:
            jp164 = false
        }
        jp158 = jp164
    default:
        panic("non-exhaustive match")
    }
    retv156 = jp158
    return retv156
}

func print__T_bool(value__0 bool) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv169 string
    var t170 string = _goml_runtime_core_bool_to_string(self__37)
    retv169 = t170
    return retv169
}

func main() {
    main0()
}
