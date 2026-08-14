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
    var x187 Color = Blue
    var x188 Color = Red
    switch x188 {
    case Red:
        switch x187 {
        case Red:
            var inline207 int = 1
            var inline208 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline207)
            _goml_runtime_core_string_print(inline208)
            return struct{}{}
        default:
            var inline211 int = 3
            var inline212 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline211)
            _goml_runtime_core_string_print(inline212)
            return struct{}{}
        }
    case Green:
        switch x187 {
        case Red:
            var inline215 int = 0
            var inline216 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline215)
            _goml_runtime_core_string_print(inline216)
            return struct{}{}
        default:
            var inline219 int = 3
            var inline220 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline219)
            _goml_runtime_core_string_print(inline220)
            return struct{}{}
        }
    case Blue:
        switch x187 {
        case Blue:
            var inline223 int = 2
            var inline224 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline223)
            _goml_runtime_core_string_print(inline224)
            return struct{}{}
        default:
            var inline227 int = 3
            var inline228 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline227)
            _goml_runtime_core_string_print(inline228)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t205 string = _goml_runtime_core_int_to_string(self__67)
    return t205
}

func main() {
    main0()
}
