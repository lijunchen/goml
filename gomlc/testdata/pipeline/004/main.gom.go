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
    var retv72 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x68 Color = a__0._0
    var x69 Color = a__0._1
    var jp74 bool
    switch x69 {
    case Red:
        var jp76 bool
        switch x68 {
        case Red:
            jp76 = true
        default:
            jp76 = false
        }
        jp74 = jp76
    case Green:
        var jp78 bool
        switch x68 {
        case Red:
            jp78 = true
        default:
            jp78 = false
        }
        jp74 = jp78
    case Blue:
        var jp80 bool
        switch x68 {
        case Blue:
            print__T_bool(true)
            jp80 = false
        default:
            jp80 = false
        }
        jp74 = jp80
    default:
        panic("non-exhaustive match")
    }
    retv72 = jp74
    return retv72
}

func print__T_bool(value__0 bool) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv85 string
    var t86 string = _goml_runtime_core_bool_to_string(self__37)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
