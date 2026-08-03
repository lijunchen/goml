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
    var x136 Color = Blue
    var x137 Color = Blue
    switch x137 {
    case Red:
        switch x136 {
        case Red:
            return true
        default:
            return false
        }
    case Green:
        switch x136 {
        case Red:
            return true
        default:
            return false
        }
    case Blue:
        switch x136 {
        case Blue:
            var inline156 bool = true
            var inline157 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline156)
            _goml_runtime_core_string_print(inline157)
            return false
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t154 string = _goml_runtime_core_bool_to_string(self__66)
    return t154
}

func main() {
    main0()
}
