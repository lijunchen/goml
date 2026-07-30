package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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
    var x68 Color = a__0._0
    var x69 Color = a__0._1
    switch x69 {
    case Red:
        switch x68 {
        case Red:
            print__T_int(1)
        default:
            print__T_int(3)
        }
    case Green:
        switch x68 {
        case Red:
            print__T_int(0)
        default:
            print__T_int(3)
        }
    case Blue:
        switch x68 {
        case Blue:
            print__T_int(2)
        default:
            print__T_int(3)
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_int(value__0 int) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv85 string
    var t86 string = _goml_runtime_core_int_to_string(self__40)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
