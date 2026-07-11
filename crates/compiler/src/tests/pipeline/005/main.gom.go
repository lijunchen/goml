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
    var x4 Color = a__0._0
    var x5 Color = a__0._1
    switch x5 {
    case Red:
        switch x4 {
        case Red:
            print__T_int32(1)
        case Green:
            print__T_int32(3)
        case Blue:
            print__T_int32(3)
        default:
            panic("non-exhaustive match")
        }
    case Green:
        switch x4 {
        case Red:
            print__T_int32(0)
        case Green:
            print__T_int32(3)
        case Blue:
            print__T_int32(3)
        default:
            panic("non-exhaustive match")
        }
    case Blue:
        switch x4 {
        case Red:
            print__T_int32(3)
        case Green:
            print__T_int32(3)
        case Blue:
            print__T_int32(2)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv24 string
    var t25 string = _goml_runtime_core_int32_to_string(self__13)
    retv24 = t25
    return retv24
}

func main() {
    main0()
}
