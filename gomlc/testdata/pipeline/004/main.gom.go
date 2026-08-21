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

type Ordering int32

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() bool {
    var x411 Color = Blue
    var x412 Color = Blue
    switch x412 {
    case Red:
        switch x411 {
        case Red:
            return true
        default:
            return false
        }
    case Green:
        switch x411 {
        case Red:
            return true
        default:
            return false
        }
    case Blue:
        switch x411 {
        case Blue:
            var inline431 bool = true
            var inline432 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline431)
            _goml_runtime_core_string_print(inline432)
            return false
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t429 string = _goml_runtime_core_bool_to_string(self__148)
    return t429
}

func main() {
    main0()
}
