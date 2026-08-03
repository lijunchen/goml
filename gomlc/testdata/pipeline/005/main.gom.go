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
    var x136 Color = Blue
    var x137 Color = Red
    switch x137 {
    case Red:
        switch x136 {
        case Red:
            var inline156 int = 1
            var inline157 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline156)
            _goml_runtime_core_string_print(inline157)
            return struct{}{}
        default:
            var inline160 int = 3
            var inline161 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline160)
            _goml_runtime_core_string_print(inline161)
            return struct{}{}
        }
    case Green:
        switch x136 {
        case Red:
            var inline164 int = 0
            var inline165 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline164)
            _goml_runtime_core_string_print(inline165)
            return struct{}{}
        default:
            var inline168 int = 3
            var inline169 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline168)
            _goml_runtime_core_string_print(inline169)
            return struct{}{}
        }
    case Blue:
        switch x136 {
        case Blue:
            var inline172 int = 2
            var inline173 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline172)
            _goml_runtime_core_string_print(inline173)
            return struct{}{}
        default:
            var inline176 int = 3
            var inline177 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline176)
            _goml_runtime_core_string_print(inline177)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t154 string = _goml_runtime_core_int_to_string(self__69)
    return t154
}

func main() {
    main0()
}
