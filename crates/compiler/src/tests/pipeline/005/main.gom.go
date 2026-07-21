package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
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

func main0() struct{} {
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Red,
    }
    var x61 Color = a__0._0
    var x62 Color = a__0._1
    switch x62 {
    case Red:
        switch x61 {
        case Red:
            print__T_int32(1)
        default:
            print__T_int32(3)
        }
    case Green:
        switch x61 {
        case Red:
            print__T_int32(0)
        default:
            print__T_int32(3)
        }
    case Blue:
        switch x61 {
        case Blue:
            print__T_int32(2)
        default:
            print__T_int32(3)
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__41)
    retv78 = t79
    return retv78
}

func main() {
    main0()
}
