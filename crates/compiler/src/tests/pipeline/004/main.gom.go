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
    var retv65 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x61 Color = a__0._0
    var x62 Color = a__0._1
    var jp67 bool
    switch x62 {
    case Red:
        var jp69 bool
        switch x61 {
        case Red:
            jp69 = true
        default:
            jp69 = false
        }
        jp67 = jp69
    case Green:
        var jp71 bool
        switch x61 {
        case Red:
            jp71 = true
        default:
            jp71 = false
        }
        jp67 = jp71
    case Blue:
        var jp73 bool
        switch x61 {
        case Blue:
            print__T_bool(true)
            jp73 = false
        default:
            jp73 = false
        }
        jp67 = jp73
    default:
        panic("non-exhaustive match")
    }
    retv65 = jp67
    return retv65
}

func print__T_bool(value__0 bool) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv78 string
    var t79 string = _goml_runtime_core_bool_to_string(self__36)
    retv78 = t79
    return retv78
}

func main() {
    main0()
}
