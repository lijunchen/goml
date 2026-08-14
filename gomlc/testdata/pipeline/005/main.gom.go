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
    var x408 Color = Blue
    var x409 Color = Red
    switch x409 {
    case Red:
        switch x408 {
        case Red:
            var inline428 int = 1
            var inline429 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline428)
            _goml_runtime_core_string_print(inline429)
            return struct{}{}
        default:
            var inline432 int = 3
            var inline433 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline432)
            _goml_runtime_core_string_print(inline433)
            return struct{}{}
        }
    case Green:
        switch x408 {
        case Red:
            var inline436 int = 0
            var inline437 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline436)
            _goml_runtime_core_string_print(inline437)
            return struct{}{}
        default:
            var inline440 int = 3
            var inline441 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline440)
            _goml_runtime_core_string_print(inline441)
            return struct{}{}
        }
    case Blue:
        switch x408 {
        case Blue:
            var inline444 int = 2
            var inline445 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline444)
            _goml_runtime_core_string_print(inline445)
            return struct{}{}
        default:
            var inline448 int = 3
            var inline449 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline448)
            _goml_runtime_core_string_print(inline449)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t426 string = _goml_runtime_core_int_to_string(self__151)
    return t426
}

func main() {
    main0()
}
