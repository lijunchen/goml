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
    var x155 Color = Blue
    var x156 Color = Red
    switch x156 {
    case Red:
        switch x155 {
        case Red:
            print__T_int(1)
            return struct{}{}
        default:
            print__T_int(3)
            return struct{}{}
        }
    case Green:
        switch x155 {
        case Red:
            print__T_int(0)
            return struct{}{}
        default:
            print__T_int(3)
            return struct{}{}
        }
    case Blue:
        switch x155 {
        case Blue:
            print__T_int(2)
            return struct{}{}
        default:
            print__T_int(3)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func print__T_int(value__0 int) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t173 string = _goml_runtime_core_int_to_string(self__40)
    return t173
}

func main() {
    main0()
}
