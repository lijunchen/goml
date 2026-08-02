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
    var x155 Color = Blue
    var x156 Color = Blue
    switch x156 {
    case Red:
        switch x155 {
        case Red:
            return true
        default:
            return false
        }
    case Green:
        switch x155 {
        case Red:
            return true
        default:
            return false
        }
    case Blue:
        switch x155 {
        case Blue:
            print__T_bool(true)
            return false
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func print__T_bool(value__0 bool) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t173 string = _goml_runtime_core_bool_to_string(self__37)
    return t173
}

func main() {
    main0()
}
