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

type Ordering int32

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() struct{} {
    var x411 Color = Blue
    var x412 Color = Red
    switch x412 {
    case Red:
        switch x411 {
        case Red:
            var inline431 int = 1
            var inline432 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline431)
            _goml_runtime_core_string_print(inline432)
            return struct{}{}
        default:
            var inline435 int = 3
            var inline436 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline435)
            _goml_runtime_core_string_print(inline436)
            return struct{}{}
        }
    case Green:
        switch x411 {
        case Red:
            var inline439 int = 0
            var inline440 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline439)
            _goml_runtime_core_string_print(inline440)
            return struct{}{}
        default:
            var inline443 int = 3
            var inline444 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline443)
            _goml_runtime_core_string_print(inline444)
            return struct{}{}
        }
    case Blue:
        switch x411 {
        case Blue:
            var inline447 int = 2
            var inline448 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline447)
            _goml_runtime_core_string_print(inline448)
            return struct{}{}
        default:
            var inline451 int = 3
            var inline452 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline451)
            _goml_runtime_core_string_print(inline452)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t429 string = _goml_runtime_core_int_to_string(self__151)
    return t429
}

func main() {
    main0()
}
