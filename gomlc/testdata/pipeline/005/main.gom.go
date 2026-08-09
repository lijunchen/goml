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

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() struct{} {
    var x172 Color = Blue
    var x173 Color = Red
    switch x173 {
    case Red:
        switch x172 {
        case Red:
            var inline192 int = 1
            var inline193 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline192)
            _goml_runtime_core_string_print(inline193)
            return struct{}{}
        default:
            var inline196 int = 3
            var inline197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline196)
            _goml_runtime_core_string_print(inline197)
            return struct{}{}
        }
    case Green:
        switch x172 {
        case Red:
            var inline200 int = 0
            var inline201 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline200)
            _goml_runtime_core_string_print(inline201)
            return struct{}{}
        default:
            var inline204 int = 3
            var inline205 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline204)
            _goml_runtime_core_string_print(inline205)
            return struct{}{}
        }
    case Blue:
        switch x172 {
        case Blue:
            var inline208 int = 2
            var inline209 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline208)
            _goml_runtime_core_string_print(inline209)
            return struct{}{}
        default:
            var inline212 int = 3
            var inline213 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline212)
            _goml_runtime_core_string_print(inline213)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t190 string = _goml_runtime_core_int_to_string(self__69)
    return t190
}

func main() {
    main0()
}
