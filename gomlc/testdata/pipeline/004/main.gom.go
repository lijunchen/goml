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
    var x172 Color = Blue
    var x173 Color = Blue
    switch x173 {
    case Red:
        switch x172 {
        case Red:
            return true
        default:
            return false
        }
    case Green:
        switch x172 {
        case Red:
            return true
        default:
            return false
        }
    case Blue:
        switch x172 {
        case Blue:
            var inline192 bool = true
            var inline193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline192)
            _goml_runtime_core_string_print(inline193)
            return false
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t190 string = _goml_runtime_core_bool_to_string(self__64)
    return t190
}

func main() {
    main0()
}
