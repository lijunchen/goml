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

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() bool {
    var x182 Color = Blue
    var x183 Color = Blue
    switch x183 {
    case Red:
        switch x182 {
        case Red:
            return true
        default:
            return false
        }
    case Green:
        switch x182 {
        case Red:
            return true
        default:
            return false
        }
    case Blue:
        switch x182 {
        case Blue:
            var inline202 bool = true
            var inline203 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline202)
            _goml_runtime_core_string_print(inline203)
            return false
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t200 string = _goml_runtime_core_bool_to_string(self__64)
    return t200
}

func main() {
    main0()
}
