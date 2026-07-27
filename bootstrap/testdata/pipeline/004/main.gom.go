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
    var retv68 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x64 Color = a__0._0
    var x65 Color = a__0._1
    var jp70 bool
    switch x65 {
    case Red:
        var jp72 bool
        switch x64 {
        case Red:
            jp72 = true
        default:
            jp72 = false
        }
        jp70 = jp72
    case Green:
        var jp74 bool
        switch x64 {
        case Red:
            jp74 = true
        default:
            jp74 = false
        }
        jp70 = jp74
    case Blue:
        var jp76 bool
        switch x64 {
        case Blue:
            print__T_bool(true)
            jp76 = false
        default:
            jp76 = false
        }
        jp70 = jp76
    default:
        panic("non-exhaustive match")
    }
    retv68 = jp70
    return retv68
}

func print__T_bool(value__0 bool) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t78)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv81 string
    var t82 string = _goml_runtime_core_bool_to_string(self__37)
    retv81 = t82
    return retv81
}

func main() {
    main0()
}
